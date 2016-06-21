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
import Generator

-- Simple function to create a hello message.
hello s = "Hello " ++ s

-- Tell QuickCheck that if you strip "Hello " from the start of
-- hello s you will be left with s (for any s).
prop_hello s = stripPrefix "Hello " (hello s) == Just s

-- Hello World
exeMain =
    do
    (old, new) <- readDeltaRPM "./data/ecall-delta-1.0-1.drpm"
    putStrLn ("old RPM: " ++ old)
    putStrLn ("new RPM: " ++ new)
    old_symbols <- readRPMSymbols old
    putStrLn ("old symbols: " ++ show old_symbols)
    new_symbols <- readRPMSymbols new
    putStrLn ("new symbols: " ++ show new_symbols)
    hunks <- deltaRPMSymbols old_symbols new_symbols
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
#define MAIN_FUNCTION  exeMain
#endif
main = MAIN_FUNCTION

