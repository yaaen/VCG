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

infix  4 :<:, :<=:, :>:, :>=:, :=:, :/=:, :>>:
infixl 3 :/\:
infixl 2 :\/:
infixr 1 :=>:
infix  0 :<=>:

data Formula  = Formula :/\: Formula
              | Formula :\/: Formula
              | Formula :=>: Formula
              | Formula :<=>: Formula
              | Not Formula
              | Exists Term  Formula
              | Forall Term  Formula
              | TRUE
              | FALSE
              | Term :<:   Term
              | Term :>:   Term
              | Term :<=:  Term
              | Term :>=:  Term
              | Term :=:   Term
              | Term :/=:  Term

data Term  = Term (Map String Formula)

logical_var    :: String -> Term
logical_var x  = Term (singleton x TRUE)

type Env = Map String Value

data VCGExpr = ReadDeltaRPM
             | ApplyDeltaRPM
             | ReadRPMSymbols
             | DeltaRPMSymbols
             | Value :>>: Value

type Evaluator a = WriterT [Formula] (StateT Env IO) a

pvcg
    :: VCGExpr ->
       Formula ->
       Evaluator VCGExpr
pvcg ReadDeltaRPM q
    = return ReadDeltaRPM

