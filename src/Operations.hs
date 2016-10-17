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
import Data.List
import Data.Maybe
import Data.Either.Unwrap
import Data.Algorithm.Diff3
import NMParser


import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

#ifndef DONT_DEFINE_DELTA
#define DELTARPM "ecall-delta-1.0-1.drpm"
#define DELTADIR "./data/"
#endif

#ifdef SAFETY_LEVEL
#define LEVEL 2
#else
#define LEVEL 1
#endif


readDeltaRPM
    :: IO (String,String)
readDeltaRPM
    = do
      putStrLn $ show DELTADIR
      putStrLn $ show DELTARPM
      let f = defVV $
            "def export(x): from deltarpm import readDeltaRPM;" ++
            "d = readDeltaRPM ('" ++ DELTADIR ++ "' + x);" ++
            "d1 = d['old_nevr'];" ++
            "d2 = d['nevr'];" ++
            "return (d1,d2)"
      (a,b) <- f DELTARPM
      putStrLn $ show a
      putStrLn $ show b
      return (a,b)

resolveUndefinedSym
    :: String ->
       IO [String]
resolveUndefinedSym binary
    = do
      let cmd = "target=" ++ binary ++ ";" ++ "\n" ++
                "cd " ++ DELTADIR ++ ";" ++ "\n" ++
                "for symbol in $(nm -D $target | grep \"U \" | cut -b12-);" ++ "\n" ++
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
      return ls

applyDeltaRPM
    :: (String, String) ->
       IO (String, String)
applyDeltaRPM (old, new)
    =
#ifdef SYSTEM
       return (old, new)
#else
      do
      let cmd = "cd " ++ DELTADIR ++ "; " ++
                    "applydeltarpm -r " ++ old ++ ".i686.rpm" ++ " " ++
                        DELTARPM ++ " " ++ new ++ ".i686.rpm"
      putStrLn cmd
      (ExitSuccess, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) ""
      return (old, new)
#endif

checkRPMInstalled
    :: String ->
       IO (Maybe String)
checkRPMInstalled old
    = do
      (exit, stdout, stderr) <- readCreateProcessWithExitCode (shell ("rpm -q " ++ old)) ""
      assert (exit == ExitSuccess) (return ())
      let cmd = "rpm -ql " ++ old
      (exit, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) ""
      case exit of
        ExitSuccess   ->  putStrLn (cmd ++ " $ ") >>
                               -- mapM_ (putStrLn) (filter (isInfixOf "bin") (lines stdout)) >>
                                    return (Just (head (filter (isInfixOf "bin") (lines stdout))))
        ExitFailure _ -> return Nothing

readInstalledSymTab
    :: Maybe String ->
       IO [(Char,String)]
readInstalledSymTab binary
    = assert (isJust binary) $
       do
       (_, stdout, stderr) <- readCreateProcessWithExitCode (shell ("nm " ++ fromJust binary)) ""
       putStrLn ("nm " ++ fromJust binary ++ " $ ")
           -- >> putStrLn stdout
       return $ map (\(a,b,c) -> (b,c)) $ fromRight (parse nmParser stdout)

readRPMSymbols
    :: (String, String) ->
       IO ([(Char, String)], [(Char, String)])
readRPMSymbols (old, new)
    =
#ifdef SYSTEM
      checkRPMInstalled old >>= \path ->
            readInstalledSymTab path >>= \a ->
                     readSymTab new True >>= \b ->
                          return (a,b)
#else
        readSymTab old False >>= \a ->
            readSymTab new True >>= \b ->
                 return (a,b)
#endif

readSymTab
    :: String ->
       Bool ->
       IO [(Char,String)]
readSymTab rpm new
    = do
      let cmd1 = "cd " ++ DELTADIR ++ "; rm -rf ./usr; rpm2cpio " ++ rpm ++ ".i686.rpm" ++
                 " | cpio --quiet -idmv 2>&1"
      (ExitSuccess, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd1) ""
      --putStrLn ("rpm2cpio := stdout :" ++ stdout) >> putStrLn ("stderr :" ++ stderr)
      b <- (parseRelFile . rstrip) $ head (filter (isInfixOf "bin") (lines stdout))
      let cmd2 = "cd " ++ DELTADIR ++ "; nm " ++ fromRelFile b
      --putStrLn ("nm from file:" ++ fromRelFile b)
      (_, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd2) ""
      --putStrLn ("nm :=  stdout :" ++ stdout) >> putStrLn ("stderr :" ++ stderr)
      let Right symbols = parse nmParser stdout
      if (LEVEL == 1)
         then return $ map (\(a,b,c) -> (b,c)) symbols
         else do
              when new (putStrLn $ "Safety Level: " ++ (show LEVEL))
              undefined_ <- resolveUndefinedSym (fromRelFile b)
              mapM (\(a,b,c) -> do
                                if (not (elem c undefined_) && b=='U' && new)
                                   then do
                                        --putStr (if new then "new rpm" else "old rpm") >>
                                            --putStrLn (" => found " ++ show (b,c))
                                        return ('T', c)
                                   else return (b, c)) symbols

deltaRPMSymbols
    :: ([(Char, String)], [(Char, String)]) ->
       IO [Hunk (Char, String)]
deltaRPMSymbols (old, new)
    = return
        $ filter (\hunk -> case hunk of { RightChange [] -> False; RightChange (_:[]) -> True; _ -> False } )
          $ diff3 old old new

exeMain =
    do
    hunks <- readDeltaRPM >>= applyDeltaRPM >>= readRPMSymbols >>= deltaRPMSymbols
    mapM (putStrLn . show) hunks
    return ()
