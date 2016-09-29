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

resolveUndefinedSym :: IO ()
resolveUndefinedSym
    = do
      let cmd = "target=\"/usr/local/bin/ecalleu_i686\";" ++ "\n" ++
                "for symbol in $(nm -D $target | grep \"U \" | cut -b12-);" ++ "\n" ++
                "do for library in $(ldd $target | cut -d ' ' -f3- | cut -d' ' -f1);" ++ "\n" ++
                "do if [ \"$library\" != \"not\" ]; then" ++ "\n" ++
                "for lib_symbol in $(nm -D $library | grep \"T \" | cut -b12-);"  ++ "\n" ++
                "do if [ $symbol == $lib_symbol ]; then echo \"$symbol \"Z\"  $library\"; fi ;" ++ "\n" ++
                "done;" ++ "\n" ++
                "else echo \"$symbol\"...no\"\";" ++ "\n" ++
                "fi;" ++ "\n" ++
                "done;" ++ "\n" ++
                "done;"
      (ExitSuccess, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) ""
      putStrLn stdout
      return ()

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
       IO ([(Char, String)], [(Char, String)])
readRPMSymbols (old, new)
    = readSymTab old >>= \a ->
        readSymTab new >>= \b ->
            return (a,b)

readSymTab
    :: String ->
       IO [(Char,String)]
readSymTab rpm
    = do
      let cmd1 = "cd data; rm -rf ./usr; rpm2cpio " ++ rpm ++ ".i686.rpm" ++ " | cpio --quiet -idmv"
      (ExitSuccess, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd1) ""
      --putStrLn ("stdout :" ++ stdout) >> putStrLn ("stderr :" ++ stderr)
      b <- (parseRelFile . rstrip) stderr
      let cmd2 = "cd data; nm " ++ fromRelFile b
      (ExitSuccess, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd2) ""
      let Right symbols = parse nmParser stdout
      return $ map (\(a,b,c) -> (b,c)) symbols
      where
      fillout s = case s of { "" -> replicate 8 ' ' ; _ -> s }

deltaRPMSymbols
    :: ([(Char, String)], [(Char, String)]) ->
       IO [Hunk (Char, String)]
deltaRPMSymbols (old, new)
    =  return
        $ filter (\hunk -> case hunk of { RightChange _ -> True; _ -> False } )
          $ diff3 old old new

exeMain =
    do
    hunks <- readDeltaRPM >>= applyDeltaRPM >>= readRPMSymbols >>= deltaRPMSymbols
    mapM (putStrLn . show) hunks
    return ()
