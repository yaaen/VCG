{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Code
import Control.Monad
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import NMParser
import Text.Parsec.String
import Data.Algorithm.Diff3
import Python
import System.Process
import System.Exit
import Control.Exception
import Path
import Data.String.Utils

-- Simple function to create a hello message.
hello s = "Hello " ++ s

-- Tell QuickCheck that if you strip "Hello " from the start of
-- hello s you will be left with s (for any s).
prop_hello s = stripPrefix "Hello " (hello s) == Just s

-- Hello World
exeMain = do
    putStrLn (hello "World")

-- Entry point for unit tests.
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure


readDeltaRPM :: String -> IO (String,String)
readDeltaRPM = defVV $
                "def export(x): from deltarpm import readDeltaRPM;" ++
                "d = readDeltaRPM (x);" ++
                "d1 = d['old_nevr'];" ++
                "d2 = d['nevr'];" ++
                "return (d1,d2)"

extractSyms
    :: String ->
       IO [(String,String)]
extractSyms rpm
    = do
      let cmd1 = "cd data; rm -rf ./usr; rpm2cpio " ++ rpm ++ ".i686.rpm" ++ " | cpio --quiet -idmv"
      (ExitSuccess, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd1) ""
      putStrLn ("stdout :" ++ stdout) >> putStrLn ("stderr :" ++ stderr)
      b <- (parseRelFile . rstrip) stderr
      let cmd2 = "cd data; nm " ++ fromRelFile b
      (ExitSuccess, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd2) ""
      let Right symbols = parse nmParser stdout
      return $ map (\(a,b,c) -> (b,c)) symbols
      where
      fillout s = case s of { "" -> replicate 8 ' ' ; _ -> s }


parser_main = do
              (old, new) <- readDeltaRPM "./data/ecall-delta-1.0-1.drpm"
              putStrLn ("old RPM: " ++ old)
              putStrLn ("new RPM: " ++ new)
              old_symbols <- extractSyms old
              putStrLn ("old symbols: " ++ show old_symbols)
              new_symbols <- extractSyms new
              putStrLn ("new symbols: " ++ show new_symbols)
              let h  = diff3 old_symbols old_symbols new_symbols
                  h' = filter (\hunk -> case hunk of { RightChange _ -> True; _ -> False } ) h
              mapM (putStrLn . show) h'
              return ()



-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION  parser_main
#endif
main = MAIN_FUNCTION

