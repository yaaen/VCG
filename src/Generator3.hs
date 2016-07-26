{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Generator3
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

module Generator3 (example) where

import Prelude hiding (lookup, filter, map)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Applicative hiding (empty)
import Data.Map
import qualified Data.List as L
import Data.Maybe
import Functions
import System.IO.Unsafe
import Extraction

#ifndef DELTA_RPM
#define DELTARPM "ecall-delta-1.0-1.drpm"
#endif

infix  4 :<:, :<=:, :>:, :>=:, :=:, :/=:
infixl 3 :/\:
infixl 2 :\/:
infixr 1 :=>:, :>>:
infix  0 :<=>:

-- meta-functions
type Var = String
data UninterFun = ReadDeltaRPM
                | ApplyDeltaRPM Var
                | ReadRPMSymbols Var
                | DeltaRPMSymbols Var
                | ComputeDelta Var
                | ProductCore
                deriving (Show)

data MetaExpr = Fun UninterFun
              | Assign Var MetaExpr
              | MetaExpr :>>: MetaExpr
              deriving (Show)

data Term = Var String | Val UninterFun
          deriving (Show)

data Formula  = Formula :/\: Formula
              | Formula :\/: Formula
              | Formula :=>: Formula
              | Formula :<=>: Formula
              | Not Formula
              | Exists String Formula
              | Forall String Formula
              | TRUE
              | FALSE
              | Term :<:   Term
              | Term :>:   Term
              | Term :<=:  Term
              | Term :>=:  Term
              | Term :=:   Term
              | Term :/=:  Term
              | PTy Term
              | PEx Term Term
              | PEv Term Term Term
              deriving (Show)

post = Forall "h"
           (PEv (Val ProductCore) (Val (ComputeDelta "h")) (Var "h"))
                :=>: (PEx (Val ProductCore) (Val (ComputeDelta "h")))
                    :=>: (PTy (Var "h"))

{-
data MetaValue = CoqTerm Value
               | RPM String
               | Tuple (MetaValue, MetaValue)
               | Undefined
               deriving (Show)
-}

data IValue = I (IO Value) | F String (Value -> IO Value) deriving (Show)

instance Show (IO Value) where
  show v = show $ unsafePerformIO v

instance Show (Value -> IO Value) where
  show v = "function"

def = I (return Undefined)

type Env  = Map String (IValue)
type VCG a = StateT Env (IO) a
runVCG :: VCG a -> Env ->  IO (a, Env)
runVCG g pre = runStateT g pre

dependency
    :: IValue ->
       String ->
       VCG ()
dependency (I val) var
    = filter isArgOf <$> get >>= \singleton ->
        if size singleton == 1
          then do
               let (v, F var f) = elemAt 0 singleton
                   val' = I (val >>= f)
               put =<< insert v val' <$> get
               dependency val' v
          else return ()
      where
        isArgOf value = case value of { I _ -> False; F var' _ -> var == var' }


pvcg
    :: MetaExpr ->
       Formula ->
       VCG Formula
pvcg (Assign "x" (Fun ReadDeltaRPM))
    = \pre -> do
              liftIO $ putStrLn (show $ ReadDeltaRPM)
              (put =<< insert "x" (I readDeltaRPM_) <$> get)
                >> dependency (I readDeltaRPM_) "x"
                >> return pre

pvcg (Assign "y" (Fun (ApplyDeltaRPM "x")))
    = \pre -> do
              liftIO $ putStrLn (show $ ApplyDeltaRPM "x")
              (put =<< insert "y" (F "x" applyDeltaRPM_) <$> get)
                >> return pre

pvcg (Assign "z" (Fun (ReadRPMSymbols "y")))
    = \pre -> do
              liftIO $ putStrLn (show $ ReadRPMSymbols "y")
              (put =<< insert "z" (F "y" readRPMSymbols_) <$> get)
                >> return pre

pvcg (Assign "h" (Fun (DeltaRPMSymbols  "z")))
    = \pre -> do
              liftIO $ putStrLn (show $ DeltaRPMSymbols "z")
              (put =<< insert "h" (F "z" deltaRPMSymbols_) <$> get)
                >> return pre

pvcg (a :>>: b) = \pre -> pvcg b pre >>= pvcg a

example = do
          let a = Assign "x" (Fun ReadDeltaRPM)
              b = Assign "y" (Fun (ApplyDeltaRPM "x"))
              c = Assign "z" (Fun (ReadRPMSymbols "y"))
              d = Assign "h" (Fun (DeltaRPMSymbols "z"))
              prog =  a :>>: (b :>>: (c :>>: d))
          (wp, env) <-runVCG (pvcg prog post) (empty)
          putStrLn (show wp)
          mapM (putStrLn . show) (toList env)

