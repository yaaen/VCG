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
import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import NMParser
import Text.Parsec.String
import Data.Algorithm.Diff3

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


parser_main = do
              symbols_europe <- parseFromFile nmParser "./data/symbols_europe.txt"
              symbols_russia <- parseFromFile nmParser "./data/symbols_russia.txt"
              let europa = case symbols_europe of
                                    --Right syms -> map (\(a,b,c)-> (format a ++ " " ++ b ++ " " ++ c)) syms
                                    Right syms -> map (\(a,b,c)-> c) syms
                                    Left err -> error (show err)
              let russia = case symbols_russia of
                                    --Right syms -> map (\(a,b,c)-> (format a ++ " " ++ b ++ " " ++ c)) syms
                                    Right syms -> map (\(a,b,c)-> c) syms
                                    Left err -> error (show err)
              --mapM (putStrLn . show) (zip europe russia)
              let h = diff3 europa europa russia
              {-let f h = case h of
                            LeftChange a -> putStrLn (show a)
                            RightChange a -> putStrLn (show a)
                            Unchanged a -> putStrLn (show a)
                            Conflict a b c -> putStrLn (a ++ "->" ++ b ++ "->" ++ c) -}
              mapM (\hunk -> putStrLn (show hunk)) (filter (\hunk -> case hunk of { RightChange _ -> True; _ -> False } ) h)
              return ()
              where
                format s = case s of { "" -> replicate 8 ' ' ; _ -> s }


-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION parser_main
#endif
main = MAIN_FUNCTION

