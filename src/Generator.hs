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
type Env  = Map String Value

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

type Eval a = (StateT (Env, [String]) IO) a
runEnv :: Eval a -> (Env, [String])-> IO (a, (Env, [String]))
runEnv = runStateT

eval :: Env -> Formula -> Eval Value
eval env (Forall var f)                           = eval env f
eval env (TRUE :/\: b)                            = eval env b
eval env (a :/\: TRUE)                            = eval env a
eval env (a :/\: b)                               = do
                                                    val_a <- eval env a
                                                    (env', unresolved) <- get
                                                    if (length unresolved /= 0)
                                                        then eval env' b >> eval env' a
                                                        else eval env' b
eval env (Var "x" :=: Val ReadDeltaRPM)           = do
                                                    (env, unresolved) <- get
                                                    output <- liftIO $ readDeltaRPM_
                                                    put $ (insert "x" output env, unresolved)
                                                    return output
eval env (Var "h" :=: Val (DeltaRPMSymbols "z"))  = do
                                                    (env, unresolved) <- get
                                                    if not (member "z" env)
                                                       then put (env, "z":unresolved) >> return Nil
                                                       else do
                                                            let input = fromJust (lookup "z" env)
                                                            output <- liftIO $ deltaRPMSymbols_ input
                                                            put (insert "h" output env, unresolved)
                                                            return output
eval env (Var "z" :=: Val (ReadRPMSymbols "y"))   = do
                                                    (env, unresolved) <- get
                                                    if not (member "y" env)
                                                       then put (env, "y":unresolved) >> return Nil
                                                       else do
                                                            let input = fromJust (lookup "y" env)
                                                            output <- liftIO $ readRPMSymbols_ input
                                                            put (insert "z" output env, unresolved)
                                                            return output

eval env (Var "y" :=: Val (ApplyDeltaRPM "x"))    = do
                                                    (env, unresolved) <- get
                                                    if not (member "x" env)
                                                       then put (env, "x":unresolved) >> return Nil
                                                       else do
                                                            let input = fromJust (lookup "x" env)
                                                            output <- liftIO $ applyDeltaRPM_ input
                                                            put (insert "y" output env, unresolved)
                                                            return output

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
          (value, (env, unresolved)) <- runEnv (eval empty wp) (empty, [])
          putStrLn ("value " ++ (show value))
          putStrLn ("unresolved " ++ (show unresolved))
          putStrLn ("x:= " ++ (show (lookup "x" env)))
          putStrLn ("y:= " ++ (show (lookup "y" env)))
          putStrLn ("z:= " ++ (show (lookup "z" env)))
          putStrLn ("h:= " ++ (show (lookup "h" env)))
          return $ lookup "h" env

--Forall "h"
    --(Forall "z"
        --(Forall "y"
            --(Forall "x"
                --((((TRUE :/\:
                    --Var "h" :=: Val (DeltaRPMSymbols "z")) :/\:
                    --Var "z" :=: Val (ReadRPMSymbols "y")) :/\:
                    --Var "y" :=: Val (ApplyDeltaRPM "x")) :/\:
                    --Var "x" :=: Val ReadDeltaRPM))))

prop_wp = unsafePerformIO example == Just (ListSymbols [Symbol "T" "asnintdecode_mobiledef",
                                                        Symbol "T" "decode_additionaldata",
                                                        Symbol "T" "decode_crashdef",
                                                        Symbol "T" "decode_testresultdef",
                                                        Symbol "T" "eCallIfaceFunc_encode_optionaldata",
                                                        Symbol "T" "eCallIfaceFunc_init_additionaldata",
                                                        Symbol "T" "eCallIfaceFunc_init_eraGlonass_data",
                                                        Symbol "T" "encode_additionaldata",
                                                        Symbol "T" "encode_crashdef",
                                                        Symbol "T" "encode_mobiledef",
                                                        Symbol "T" "encode_testresultdef",
                                                        Symbol "D" "ID_ADDITIONALDATA",
                                                        Symbol "T" "init_additionaldata",
                                                        Symbol "T" "init_crashdef",
                                                        Symbol "T" "init_mobiledef",
                                                        Symbol "T" "init_testresultdef",
                                                        Symbol "D" "OID_ADDITIONALDATA"])
