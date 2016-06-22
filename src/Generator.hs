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
import Data.IntMap
import qualified Data.Map as M
import Code

-- old rpm    : 1
-- new rpm    : 2
-- old symtab : 3
-- new symtab : 4
-- delta diff : 5
type Name  = Int

data Term  = Term (IntMap Integer) Integer

var        :: Name -> Term
var x      = Term (singleton x 1) 0

num        :: Integer -> Term
num n      = Term empty n

infixl 3 :/\:
infixl 2 :\/:
infixr 1 :=>:
infix  0 :<=>:

infix 4 :<:, :<=:, :>:, :>=:, :=:, :/=:, :>>:

data Formula  = Formula :/\: Formula
              | Formula :\/: Formula
              | Formula :=>: Formula
              | Formula :<=>: Formula
              | Not Formula
              | Exists (Term -> Formula)
              | Forall (Term -> Formula)
              | TRUE
              | FALSE
              | Term :<:   Term
              | Term :>:   Term
              | Term :<=:  Term
              | Term :>=:  Term
              | Term :=:   Term
              | Term :/=:  Term

type Env = M.Map String Value

data VCGExpr = ReadDeltaRPM
             | ApplyDeltaRPM
             | ReadRPMSymbols
             | DeltaRPMSymbols
             | Value :>>: Value

type Evaluator a = WriterT [Formula] (StateT Env IO) a

pvcg :: VCGExpr -> Formula -> Evaluator VCGExpr
pvcg ReadDeltaRPM q = return ReadDeltaRPM

