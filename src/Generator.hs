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

module Generator (

) where

import Control.Monad.State
import Control.Monad.Writer
import Data.Map
import Code
import Functions

#ifndef DELTA_RPM
#define DELTARPM "ecall-delta-1.0-1.drpm"
#endif

infix  4 :<:, :<=:, :>:, :>=:, :=:, :/=:
infixl 3 :/\:, :>>:
infixl 2 :\/:
infixr 1 :=>:
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
type Env = Map Term Value

data VCGExpr = ReadDeltaRPM
             | ApplyDeltaRPM
             | ReadRPMSymbols
             | DeltaRPMSymbols
             | VCGExpr :>>: VCGExpr

type VCG a = WriterT [Formula] (StateT Env IO) a
runVCG :: Env -> [Formula] -> VCG a -> IO ((a, [Formula]), Env)
runVCG env vcs g = runStateT (runWriterT g) env

pvcg :: VCGExpr -> Formula -> VCG ()
pvcg ReadDeltaRPM q
    = do { Tuple (RPM a, RPM b) <- liftIO readDeltaRPM_ ; return () }
pvcg ApplyDeltaRPM q
    = liftIO $ applyDeltaRPM_  (Tuple (RPM "", RPM "")) >> return ()
pvcg ReadRPMSymbols q
    = do { Tuple (a, b) <- liftIO $ readRPMSymbols_ (Tuple (RPM "", RPM "")); return () }
pvcg DeltaRPMSymbols q
    = do
      Tuple (RPM a, RPM b) <- liftIO $ deltaRPMSymbols_ (Tuple (ListSymbols [], ListSymbols []))
      return ()

example = let var = "x" :: Term
              prog = ReadDeltaRPM :>>: ApplyDeltaRPM :>>: ReadRPMSymbols :>>: DeltaRPMSymbols
              q = Forall var TRUE
          in runVCG (singleton var (RPM DELTARPM)) [] (pvcg prog q)

