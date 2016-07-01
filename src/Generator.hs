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

module Generator (example, prop_wp) where

import Prelude hiding (lookup)
import Control.Monad.State
import Control.Monad.Writer
import Data.Map
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
                deriving (Show)

data Term = Var String
          | Val UninterFun
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
              deriving (Show)

type Pre  = Formula
type Env  = Map String (Value)

data VCGExpr = Fun UninterFun
             | Assign Var VCGExpr
             | VCGExpr :>>: VCGExpr
             deriving (Show)


type VCG a = WriterT [Formula] (StateT Pre IO) a
runVCG :: Pre -> VCG a -> IO ((a, [Formula]), Pre)
runVCG pre g = runStateT (runWriterT g) pre

pvcg
    :: VCGExpr ->
       Formula ->
       VCG ()
pvcg (Assign "x" (Fun ReadDeltaRPM)) _
    = do
      liftIO $ putStrLn (show $ ReadDeltaRPM)
      (Forall "h" (Forall "z" (Forall "y" (Forall "x" assign)))) <- get
      put (Forall "h" (Forall "z" (Forall "y" (Forall "x" (assign :/\: (Var "x" :=: Val ReadDeltaRPM))))))

pvcg (Assign "y" (Fun (ApplyDeltaRPM "x"))) _
    = do
      liftIO $ putStrLn (show $ ApplyDeltaRPM "x")
      (Forall "h" (Forall "z" (Forall "y" assign))) <- get
      put (Forall "h" (Forall "z" (Forall "y" (Forall "x" (assign :/\: (Var "y" :=: Val (ApplyDeltaRPM "x")))))))

pvcg (Assign "z" (Fun (ReadRPMSymbols "y"))) _
    = do
      liftIO $ putStrLn (show $ ReadRPMSymbols "y")
      (Forall "h" (Forall "z" assign)) <- get
      put (Forall "h" (Forall "z" (Forall "y" (assign :/\: (Var "z" :=: Val (ReadRPMSymbols "y"))))))

pvcg (Assign "h" (Fun (DeltaRPMSymbols  "z"))) _
    = do
      liftIO $ putStrLn (show $ DeltaRPMSymbols "z")
      Forall "h" (Forall "z" (Forall "y" (Forall "x" pre))) <- get
      put (Forall "h" (Forall "z" (pre :/\: (Var "h" :=: Val (DeltaRPMSymbols "z")))))

pvcg (a :>>: b) q
    = do
      b' <- pvcg b q
      a' <- pvcg a q
      return a'

type Eval a = WriterT [String] (StateT Env IO) a
runEnv :: Eval a->  Env ->  IO ((a, [String]), Env)
runEnv eval env = runStateT (runWriterT eval) env

eval :: [String] -> Formula -> Eval ([String])
eval unresolved (Forall var f)
    = do
      (env, unresolved) <- get
      let env' = insert var Empty env
      put (env', unresolved)
      eval env' f
eval unresolved (TRUE :/\: b)
    = do
      (env', unresolved) <- get
      eval env' b
eval unresolved (a :/\: TRUE)
    = do
      (env', unresolved) <- get
      eval env' a
eval unresolved (a :/\: b)
    = do
      (env, _) <- get
      val_a <- eval env a
      (env', unresolved) <- get
      if (length unresolved /= 0)
         then eval env' b >> eval env' a
         else eval env' b
eval unresolved (Var "x" :=: Val ReadDeltaRPM)
    = do
      (env, unresolved) <- get
      output <- liftIO $ readDeltaRPM_
      let env' = adjust (\_ -> output) "x" env
      put (env', unresolved)
eval _ (Var "h" :=: Val (DeltaRPMSymbols "z"))
    = do
      (env, unresolved) <- get
      if (lookup "z" env) == Just Empty
         then do
              put (env, "z":unresolved)
         else do
              let input = fromJust (lookup "z" env)
              output <- liftIO $ deltaRPMSymbols_ input
              let env' = adjust (\_ -> output) "h" env
              put (env', unresolved)
eval _ (Var "z" :=: Val (ReadRPMSymbols "y"))
    = do
      (env, unresolved) <- get
      if (lookup "y" env) == Just Empty
         then put (env, "y":unresolved)
         else do
              let input = fromJust (lookup "y" env)
              output <- liftIO $ readRPMSymbols_ input
              put (adjust (\_ -> output) "z" env, unresolved)
eval _ (Var "y" :=: Val (ApplyDeltaRPM "x"))
    = do
      (env, unresolved) <- get
      if (lookup "x" env) == Just Empty
           then put (env, "x":unresolved)
           else do
                let input = fromJust (lookup "x" env)
                output <- liftIO $ applyDeltaRPM_ input
                put (adjust (\_ -> output) "y" env, unresolved)

example = do
          let a = Assign "x" (Fun ReadDeltaRPM)
              b = Assign "y" (Fun (ApplyDeltaRPM "x"))
              c = Assign "z" (Fun (ReadRPMSymbols "y"))
              d = Assign "h" (Fun (DeltaRPMSymbols "z"))
              prog =  a :>>: (b :>>: (c :>>: d))
              pre = Forall "h" (Forall "z" (Forall "y" (Forall "x" TRUE)))
              q = TRUE
          post <- exeMain_
          ((expr, []), wp) <-runVCG pre (pvcg prog q)
          putStrLn (show wp)
          (_, (env, unresolved)) <- runEnv (eval empty wp) (empty, [])
          putStrLn ("unresolved " ++ (show unresolved))
          putStrLn ("x:= " ++ (show (lookup "x" env)))
          putStrLn ("y:= " ++ (show (lookup "y" env)))
          putStrLn ("z:= " ++ (show (lookup "z" env)))
          putStrLn ("h:= " ++ (show (lookup "h" env)))
          putStrLn $ show env
          return $ fromJust $ lookup "h" env

--Forall "h"
    --(Forall "z"
        --(Forall "y"
            --(Forall "x"
                --((((TRUE :/\:
                    --Var "h" :=: Val (DeltaRPMSymbols "z")) :/\:
                    --Var "z" :=: Val (ReadRPMSymbols "y")) :/\:
                    --Var "y" :=: Val (ApplyDeltaRPM "x")) :/\:
                    --Var "x" :=: Val ReadDeltaRPM))))

prop_wp = let a = unsafePerformIO example
              b = unsafePerformIO exeMain_
          in a == b










