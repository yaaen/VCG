{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  Generator
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

module Generator (example) where

import Prelude hiding (lookup)
import Control.Monad.State
import Control.Monad.Writer
import Data.Map
import Data.Maybe
import Code
import Functions

#ifndef DELTA_RPM
#define DELTARPM "ecall-delta-1.0-1.drpm"
#endif

infix  4 :<:, :<=:, :>:, :>=:, :=:, :/=:
infixl 3 :/\:
infixl 2 :\/:
infixr 1 :=>:, :>>:
infix  0 :<=>:

type Term = String
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

type Pre  = Map Term Formula

type Var = String
type Env = Map Var Value

data VCGExpr = ReadDeltaRPM
             | ApplyDeltaRPM Var
             | ReadRPMSymbols Var
             | DeltaRPMSymbols Var
             | Assign Var VCGExpr
             | VCGExpr :>>: VCGExpr
             deriving (Show)


type VCG a = WriterT [Formula] (StateT Env IO) a
runVCG :: Env -> [Formula] -> VCG a -> IO ((a, [Formula]), Env)
runVCG env vcs g = runStateT (runWriterT g) env

pvcg
    :: VCGExpr ->
       Formula ->
       VCG ()
pvcg (Assign var ReadDeltaRPM) _
    = do
      env <- get
      value <- liftIO readDeltaRPM_
      put (insert var value env)

pvcg (Assign var (ApplyDeltaRPM arg)) _
    = do
      env <- get
      value <- liftIO $ applyDeltaRPM_ (fromJust (lookup arg env))
      put (insert var value env)

pvcg (Assign var (ReadRPMSymbols arg)) _
    = do
      env <- get
      value <- liftIO $ readRPMSymbols_ (fromJust (lookup arg env))
      put (insert var value env)

pvcg (Assign "h" (DeltaRPMSymbols "z")) _
    = do
      env <- get
      value <- liftIO $ deltaRPMSymbols_ (fromJust (lookup "z" env))
      put (insert "h" value env)

pvcg (a :>>: b) q
    = do
      b' <- pvcg b q
      a' <- pvcg a q
      return a'

example = do
          let a = Assign "x" ReadDeltaRPM
              b = Assign "y" (ApplyDeltaRPM "x")
              c = Assign "z" (ReadRPMSymbols "y")
              d = Assign "h" (DeltaRPMSymbols "z")
              prog =  a :>>: (b :>>: (c :>>: d))
              q = TRUE
          post <- exeMain_
          runVCG (singleton "d" post) [] (pvcg prog q)
