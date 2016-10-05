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


import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

#ifndef DELTA_RPM
#define DELTARPM "ecall-delta-1.0-1.drpm"
#endif

#ifndef SAFETY_LEVEL
#define SAFETYLEVEL "shared"
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

resolveUndefinedSym
    :: String ->
       IO [String]
resolveUndefinedSym binary
    = do
      let cmd = "target=" ++ binary ++ ";" ++ "\n" ++
                "cd data; for symbol in $(nm -D $target | grep \"U \" | cut -b12-);" ++ "\n" ++
                "do for library in $(ldd $target | cut -d ' ' -f3- | cut -d' ' -f1);" ++ "\n" ++
                "do if [ \"$library\" != \"not\" ]; then" ++ "\n" ++
                "for lib_symbol in $(nm -D $library | grep \"T \" | cut -b12-);"  ++ "\n" ++
                "do if [ $symbol == $lib_symbol ]; then echo -n \"\"; fi ;" ++ "\n" ++
                "done;" ++ "\n" ++
                "else echo $symbol;" ++ "\n" ++
                "fi;" ++ "\n" ++
                "done;" ++ "\n" ++
                "done;"
      (ExitSuccess, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) ""
      let ls = lines stdout
      putStrLn "Undefined: ========="
      mapM putStrLn ls
      putStrLn "===================="
      return ls

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
    = readSymTab old False >>= \a ->
        readSymTab new True >>= \b ->
            return (a,b)

readSymTab
    :: String ->
       Bool ->
       IO [(Char,String)]
readSymTab rpm new
    = do
      let cmd1 = "cd data; rm -rf ./usr; rpm2cpio " ++ rpm ++ ".i686.rpm" ++
                 " | cpio --quiet -idmv 2>&1"
      (ExitSuccess, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd1) ""
      --putStrLn ("rpm2cpio := stdout :" ++ stdout) >> putStrLn ("stderr :" ++ stderr)
      b <- (parseRelFile . rstrip) ((lines stdout)!!0)
      let cmd2 = "cd data; nm " ++ fromRelFile b
      --putStrLn ("nm from file:" ++ fromRelFile b)
      (_, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd2) ""
      --putStrLn ("nm :=  stdout :" ++ stdout) >> putStrLn ("stderr :" ++ stderr)
      let Right symbols = parse nmParser stdout
      if (SAFETYLEVEL /= "shared")
         then return $ map (\(a,b,c) -> (b,c)) symbols
         else do
              undefined_ <- resolveUndefinedSym (fromRelFile b)
              return $ map (\(a,b,c) -> if (elem [b] undefined_ && b=='U' && new)
                                           then ('u',c)
                                           else if (not (elem [b] undefined_) && b=='U' && new)
                                                then ('T',c)
                                                else (b,c)) symbols
      where
      fillout s = case s of { "" -> replicate 8 ' ' ; _ -> s }




deltaRPMSymbols
    :: ([(Char, String)], [(Char, String)]) ->
       IO [Hunk (Char, String)]
deltaRPMSymbols (old, new)
    = do
      return
        $ filter (\hunk -> case hunk of { RightChange [] -> False; RightChange (_:[]) -> True; _ -> False } )
          $ diff3 old old new

exeMain =
    do
    hunks <- readDeltaRPM >>= applyDeltaRPM >>= readRPMSymbols >>= deltaRPMSymbols
    mapM (putStrLn . show) hunks
    return ()
