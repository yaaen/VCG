{-# LANGUAGE CPP, TemplateHaskell, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Operations
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

module Operations (readDeltaRPM, applyDeltaRPM, readRPMSymbols, deltaRPMSymbols, exeMain) where

import Text.Parsec.String
import Python
import System.Process
import System.Exit
import Control.Exception
import Control.Monad
import Control.Applicative
import Path
import Data.String.Utils
import Data.Algorithm.Diff3
import NMParser
import Code

#ifndef DELTA_RPM
#define DELTARPM "ecall-delta-1.0-1.drpm"
#endif

readDeltaRPM
    :: IO (String,String)
readDeltaRPM
    = let f = defVV $
            "def export(x): from deltarpm import readDeltaRPM;" ++
            "d = readDeltaRPM ('./data/' + x);" ++
            "d1 = d['old_nevr'];" ++
            "d2 = d['nevr'];" ++
            "return (d1,d2)"
      in f DELTARPM

applyDeltaRPM
    :: (String, String) ->
       IO (String, String)
applyDeltaRPM (old, new)
    = do
      let cmd = "cd data; " ++
                    "applydeltarpm -r " ++ old ++ ".i686.rpm" ++ " " ++
                        DELTARPM ++ " " ++ new ++ ".i686.rpm"
      (ExitSuccess, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) ""
      return (old, new)

readRPMSymbols
    :: (String, String) ->
       IO ([(String,String)], [(String,String)])
readRPMSymbols (old, new)
    = readSymTab old >>= \a ->
        readSymTab new >>= \b ->
            return (a,b)

readSymTab
    :: String ->
       IO [(String,String)]
readSymTab rpm
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

deltaRPMSymbols
    :: ([(String,String)], [(String,String)]) ->
       IO [Hunk (String, String)]
deltaRPMSymbols (old, new)
    =  return
        $ filter (\hunk -> case hunk of { RightChange _ -> True; _ -> False } )
          $ diff3 old old new

exeMain =
    do
    hunks <- readDeltaRPM >>= applyDeltaRPM >>= readRPMSymbols >>= deltaRPMSymbols
    mapM (putStrLn . show) hunks
    return ()
