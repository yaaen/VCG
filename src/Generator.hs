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
import Control.Applicative hiding (empty)
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
              | TypedValue Term
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
      tell [TypedValue (Var "h")]
      put (Forall "h" (Forall "z" (pre :/\: (Var "h" :=: Val (DeltaRPMSymbols "z")))))

pvcg (a :>>: b) q
    = do
      b' <- pvcg b q
      a' <- pvcg a q
      return a'

type Eval a = WriterT [String] (StateT Env IO) a
runEnv :: Eval a->  Env ->  IO ((a, [String]), Env)
runEnv eval env = runStateT (runWriterT eval) env

eval :: Formula -> Eval ([String])
eval (Forall var f)
    = insert var Empty <$> get >>= \env' ->
        put env' >>
          eval f
eval (TRUE :/\: b)  = eval b
eval (a :/\: TRUE)  = eval a
eval (a :/\: b)
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

example = do
          let a = Assign "x" (Fun ReadDeltaRPM)
              b = Assign "y" (Fun (ApplyDeltaRPM "x"))
              c = Assign "z" (Fun (ReadRPMSymbols "y"))
              d = Assign "h" (Fun (DeltaRPMSymbols "z"))
              prog =  a :>>: (b :>>: (c :>>: d))
              pre = Forall "h" (Forall "z" (Forall "y" (Forall "x" TRUE)))
              q = TRUE
          post <- exeMain_
          ((expr, vcgs), wp) <-runVCG pre (pvcg prog q)
          putStrLn (show wp)
          putStrLn (show vcgs)
          (unresolved, env) <- runEnv (eval wp) (empty)
          --putStrLn ("unresolved " ++ (show unresolved))
          --putStrLn ("x:= " ++ (show (lookup "x" env)))
          --putStrLn ("y:= " ++ (show (lookup "y" env)))
          --putStrLn ("z:= " ++ (show (lookup "z" env)))
          putStrLn ("h:= " ++ (show (lookup "h" env)))
          --putStrLn $ show env
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










