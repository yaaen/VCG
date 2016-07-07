{-# LANGUAGE CPP #-}{-# LANGUAGE ScopedTypeVariables #-}
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

module Generator (example, prop_wp) where

import Prelude hiding (lookup)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Applicative hiding (empty)
import Data.Map
import qualified Data.List as L
import Data.Maybe
import Code
import Functions
import System.IO.Unsafe

#ifndef DELTA_RPM
#define DELTARPM "ecall-delta-1.0-1.drpm"
#endif

infix  4 :<:, :<=:, :>:, :>=:, :=:, :/=:
infixl 3 :/\:
infixl 2 :\/:
infixr 1 :=>:, :>>:
infix  0 :<=>:

type Var = String
data UninterFun = ReadDeltaRPM
                | ApplyDeltaRPM Var
                | ReadRPMSymbols Var
                | DeltaRPMSymbols Var
                | ComputeDelta Var
                | DeltaCore
                deriving (Show)

data Term = Var String
          | Val UninterFun
          | Coq Prop
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
              | TypedValue Term
              | Evaluation Term Term Term
              | TypedExpression Term Term
              | Proposition Prop
              deriving (Show)

type Pre  = Formula
type Env  = Map String (Value)

data VCGExpr = Fun UninterFun
             | Assign Var VCGExpr
             | VCGExpr :>>: VCGExpr
             deriving (Show)


type VCG a = WriterT [Formula] (StateT Pre IO) a
runVCG :: VCG a -> Pre ->  IO ((a, [Formula]), Pre)
runVCG g pre = runStateT (runWriterT g) pre

pvcg
    :: VCGExpr ->
       VCG ()
pvcg (Assign "x" (Fun ReadDeltaRPM))
    = do
      liftIO $ putStrLn (show $ ReadDeltaRPM)
      (Forall "h" (Forall "z" (Forall "y" (Forall "x" assign)))) <- get
      put (Forall "h" (Forall "z" (Forall "y" (Forall "x" (assign :=>: (Var "x" :=: Val ReadDeltaRPM))))))

pvcg (Assign "y" (Fun (ApplyDeltaRPM "x")))
    = do
      liftIO $ putStrLn (show $ ApplyDeltaRPM "x")
      (Forall "h" (Forall "z" (Forall "y" assign))) <- get
      put (Forall "h" (Forall "z" (Forall "y" (Forall "x" (assign :=>: (Var "y" :=: Val (ApplyDeltaRPM "x")))))))

pvcg (Assign "z" (Fun (ReadRPMSymbols "y")))
    = do
      liftIO $ putStrLn (show $ ReadRPMSymbols "y")
      (Forall "h" (Forall "z" assign)) <- get
      put (Forall "h" (Forall "z" (Forall "y" (assign :=>: (Var "z" :=: Val (ReadRPMSymbols "y"))))))

pvcg (Assign "h" (Fun (DeltaRPMSymbols  "z")))
    = do
      liftIO $ putStrLn (show $ DeltaRPMSymbols "z")
      Forall "h" (Forall "z" (Forall "y" (Forall "x" pre))) <- get
      put (Forall "h" (Forall "z" (pre :=>: (Var "h" :=: Val (DeltaRPMSymbols "z")))))
      tell [Evaluation (Val DeltaCore) (Val (ComputeDelta "h")) (Var "h") :=>:
            TypedExpression (Val DeltaCore) (Val (ComputeDelta "h")) :=>:
            TypedValue (Var "h")]

pvcg (a :>>: b) = pvcg b >> pvcg a



type Eval a = WriterT [String] (StateT Env IO) a
runEnv :: Eval a->  Env ->  IO ((a, [String]), Env)
runEnv eval env = runStateT (runWriterT eval) env

eval :: Formula -> Eval ([String])
eval (Forall var f)
    = insert var Empty <$> get >>= \env' ->
        put env' >>
          eval f
eval (TRUE :=>: b)  = eval b
eval (a :=>: TRUE)  = eval a
eval (a :=>: b)
    = eval a >>= \unresolved ->
        if (length unresolved /= 0)
          then eval b >> eval a
          else eval b
eval (Var "x" :=: Val ReadDeltaRPM)
    = (liftIO readDeltaRPM_) >>= \val ->
        adjust (\Empty -> val) "x" <$> get >>= \env ->
          put env >> return []
eval (Var "h" :=: Val (DeltaRPMSymbols "z"))
    = lookup "z" <$> get >>= \input ->
        if input == Just Empty
          then return ("z":[])
          else (liftIO (deltaRPMSymbols_ (fromJust input))) >>= \val ->
                 adjust (\Empty -> val) "h" <$> get >>= \env ->
                   put env >> return []
eval (Var "z" :=: Val (ReadRPMSymbols "y"))
    = lookup "y" <$> get >>= \input ->
        if input == Just Empty
          then return ("y":[])
          else (liftIO (readRPMSymbols_ (fromJust input))) >>= \val ->
                 adjust (\Empty -> val) "z" <$> get >>= \env ->
                   put env >> return []
eval (Var "y" :=: Val (ApplyDeltaRPM "x"))
    = lookup "x" <$> get >>= \input ->
        if input == Just Empty
          then return ("x":[])
          else (liftIO (applyDeltaRPM_ (fromJust input))) >>= \val ->
                 adjust (\Empty -> val) "y" <$> get >>= \env ->
                   put env >> return []


type EvalVC a = ReaderT Env (IO) a

test = ((+) 4) 5

evalVC :: Formula -> EvalVC Prop
evalVC (a :=>: b) = liftM2 (:==>:) (evalVC a) (evalVC b)
evalVC (Evaluation (Val DeltaCore) (Val (ComputeDelta "h")) (Var "h"))
    =

      do
      env <- ask
      let ListSymbols symbols = fromJust $ lookup "h" env
      return $ CoqEvaluation (ListSymbols [])
                             (Compile (Delta (L.map (Add_operation . Object_elem) symbols)))
                             (ListSymbols symbols)
evalVC (TypedExpression (Val DeltaCore) (Val (ComputeDelta "h")))
    = do
      env <- ask
      let ListSymbols symbols = fromJust $ lookup "h" env
      return $ CoqTypedExpression (ListSymbols [])
                                  (Compile (Delta (L.map (Add_operation . Object_elem) symbols)))
evalVC (TypedValue (Var "h"))
    = do
      env <- ask
      let ListSymbols symbols = fromJust $ lookup "h" env
      return $ CoqTypedValue (ListSymbols symbols)

runVC :: EvalVC a->  Env ->  IO a
runVC eval env = runReaderT eval env

example = do
          let a = Assign "x" (Fun ReadDeltaRPM)
              b = Assign "y" (Fun (ApplyDeltaRPM "x"))
              c = Assign "z" (Fun (ReadRPMSymbols "y"))
              d = Assign "h" (Fun (DeltaRPMSymbols "z"))
              prog =  a :>>: (b :>>: (c :>>: d))
              post = Forall "h" (Forall "z" (Forall "y" (Forall "x" TRUE)))
          post_ <- exeMain_
          ((expr, vc:[]), wp) <-runVCG (pvcg prog) post
          putStrLn (show wp)
          putStrLn (show vc)
          (_, env) <- runEnv (eval wp) (empty)
          let symbols = fromJust $ lookup "h" env
          prop <- runVC (evalVC vc) env
          putStrLn $ show prop
          --putStrLn $ show symbols
          return symbols

prop_wp = let a = unsafePerformIO example
              b = unsafePerformIO exeMain_
          in a == b



{-
Forall "h" (
    Forall "z" (
        Forall "y" (
            Forall "x" ((((TRUE
                :=>: Var "h" :=: Val (DeltaRPMSymbols "z"))
                :=>: Var "z" :=: Val (ReadRPMSymbols "y"))
                :=>: Var "y" :=: Val (ApplyDeltaRPM "x"))
                :=>: Var "x" :=: Val ReadDeltaRPM))))

(Evaluation (Val DeltaCore) (Val (ComputeDelta "h")) (Var "h")
        :=>: TypedExpression (Val DeltaCore) (Val (ComputeDelta "h")))
            :=>: TypedValue (Var "h")
-}






