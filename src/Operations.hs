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

module Operations (readDeltaRPM, readRPMSymbols, deltaRPMSymbols) where

import Text.Parsec.String
import Python
import System.Process
import System.Exit
import Control.Exception
import Path
import Data.String.Utils
import Data.Algorithm.Diff3
import NMParser

readDeltaRPM
    :: String ->
       IO (String,String)
readDeltaRPM
    = defVV $
      "def export(x): from deltarpm import readDeltaRPM;" ++
      "d = readDeltaRPM (x);" ++
      "d1 = d['old_nevr'];" ++
      "d2 = d['nevr'];" ++
      "return (d1,d2)"

readRPMSymbols
    :: String ->
       IO [(String,String)]
readRPMSymbols rpm
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
    :: [(String,String)] ->
       [(String,String)] ->
       IO [Hunk (String, String)]
deltaRPMSymbols old new
    =  return
        $ filter (\hunk -> case hunk of { RightChange _ -> True; _ -> False } )
          $ diff3 old old new

