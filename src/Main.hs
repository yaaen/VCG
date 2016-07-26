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
import Data.Algorithm.Diff3
import Operations
import Generator3
import Functions
import Extraction

exeMainDebug =
    do
    (old, new) <- readDeltaRPM
    putStrLn ("old RPM: " ++ old)
    putStrLn ("new RPM: " ++ new)
    applyDeltaRPM (old, new)
    (old_symbols, new_symbols) <- readRPMSymbols (old, new)
    putStrLn ("old symbols: " ++ show old_symbols)
    putStrLn ("new symbols: " ++ show new_symbols)
    hunks <- deltaRPMSymbols (old_symbols, new_symbols)
    mapM (putStrLn . show) hunks
    return ()

-- Entry point for unit tests.
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure


-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION example --exeMain_
#endif
main = MAIN_FUNCTION

