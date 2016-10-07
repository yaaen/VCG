{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Bifunctor
import Control.Applicative hiding (empty)
import Data.Map
import Data.Typeable
import qualified Data.List as L
import Data.Maybe
import Functions
import System.IO.Unsafe
import Extraction

#ifndef DELTA_RPM
#define DELTARPM "ecall-delta-1.0-1.drpm"
#endif

infixl 3 :/\:
infixl 2 :\/:
infixr 1 :=>:, :>>:
infix  0 :<=>:

-- meta-functions
type Var = String
data UninterFun = ReadDeltaRPM Var
                | ApplyDeltaRPM Var
                | ReadRPMSymbols Var
                | DeltaRPMSymbols Var
                deriving (Show)

data MetaExpr = Fun UninterFun
              | Assign Var MetaExpr
              | MetaExpr :>>: MetaExpr
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
              | Coq Prop
              deriving (Show)

preCondition
    :: Env -> Formula
preCondition env
     = Forall "h"
         (Coq (CoqPEv (subst_ev env "h")))
            :=>: (Coq (CoqPEx (subst_ex env "h")))
                :=>: (Coq (CoqPTy (subst_ty env "h")))

resolve :: Formula -> Env -> IO Formula
resolve f = \env ->
               do
               putStrLn "====================\n RESOLVE WP \n===================="
               return $
                 Forall "h"
                    (Coq (CoqPEv (subst_ev env "h")))
                         :=>: (Coq (CoqPEx (subst_ex env "h")))
                             :=>: (Coq (CoqPTy (subst_ty env "h")))

subst_ev env "h"
    = let syms = if member "h" env
                    then case env ! "h" of
                            I (C (ListSymbols syms)) -> syms
                            F _ _ -> []
                    else []
          delta = Delta (L.map (\s -> Add_operation (Object_elem s)) (L.take 2 syms))
          value = Assemble (L.take 2 syms)
      in Semantics (delta, value)
subst_ex env "h"
    =  TypedExpression (ListSymbols [], Delta [], Type_Delta Type_Object)
subst_ty env "h"
    =  TypedValue (ListSymbols [], Type_Delta Type_Object)

{-
eval :: Formula -> Store -> IO Formula
eval (Forall _ f) s = eval f s
eval (a :=>: b) s = liftM2 ( (:=>:) ) (eval a s) (eval b s)
eval (PEv (Val ProductCore) (Val (ComputeDelta "h")) (Var "h")) s
    = do
      C (ListSymbols syms) <- s ! "h"
      let core =  ListSymbols []
          delta = Compile (Delta (L.map (\s -> Add_operation (Object_elem s)) (L.take 2 syms)))
          value = Assemble (L.take 2 syms)
      pure (Coq (CoqPEv (Semantics (core, delta, value))))
eval (PEx (Val ProductCore) (Val (ComputeDelta "h"))) s
    = do
      C (ListSymbols syms) <- s ! "h"
      let core =  ListSymbols []
          delta = Compile (Delta (L.map (\s -> Add_operation (Object_elem s)) (L.take 2 syms)))
      pure (Coq (CoqPEx (TypedExpression (core, delta, Type_Variant Type_Object))))
eval (PTy (Var "h")) s
    = do
      C (ListSymbols syms) <- s ! "h"
      pure (Coq (CoqPTy (TypedValue (Assemble (L.take 2 syms), Type_Variant Type_Object))))
-}

data IValue = I (Value) |
              F String (Value -> Value)
            deriving (Typeable)

instance Show IValue where
    showsPrec _ (I v) = (("I" ++ " " ++ show v) ++)
    showsPrec _ (F v f) = (("F " ++ v ++ " " ++ show f) ++)

--instance Show (IO Value) where
--  show v = show $ unsafePerformIO v

instance Show (Value -> Value) where
  show v = "function"

type MetaMap = Map String IValue
metaFunctions::MetaMap = fromList [("x", I _readDeltaRPM_),
                                   ("y", F "x" _applyDeltaRPM_),
                                   ("z", F "y" _readRPMSymbols_),
                                   ("h", F "z" _deltaRPMSymbols_)]

type Env  = Map String IValue
type VCG a = ReaderT MetaMap (StateT Env (IO)) a
runVCG :: VCG a -> MetaMap -> Env ->  IO (a, Env)
runVCG vcg meta env = runStateT (runReaderT vcg meta) env

dependency
    :: (String, IValue) ->
       VCG ()
dependency (var, F _ fun)
    = return ()
dependency (var, I val)
    = filter somearg <$> get >>=
        \case map | size map == 1 ->
                       pure (bimap id (\(F _ f) -> I (f val)) (dep map)) >>=
                            \kv -> (put =<< (uncurry insert) kv <$> get)
                                 *> dependency kv
                  | otherwise -> return ()
      where
        somearg value = case value of { F var' _ -> var == var' ; _ -> False }
        dep map = (head (toList map))


pvcg
    :: MetaExpr ->
       (Env -> IO Formula) ->
       VCG Formula

{-pvcg (Assign "x" (Fun (ReadDeltaRPM "_")))
    = \pre -> do
              liftIO $ putStrLn (show $ ReadDeltaRPM "_")
              let v = readDeltaRPM_
              let n = tyConName (typeRepTyCon (typeOf v))
              let m = if n == "->" then Just (I v) else Nothing
              (put =<< insert "x" (Just (I v)) <$> get)
                >> dependency ("x", (Just (I v)))
                >> pre <$> get

pvcg (Assign "y" (Fun (ApplyDeltaRPM "x")))
    = \pre -> do
              liftIO $ putStrLn (show $ ApplyDeltaRPM "x")
              let f = applyDeltaRPM_
              let n = tyConName (typeRepTyCon (typeOf f))
              let v = if n == "->" then Just (F "x" f) else Nothing
              (put =<< insert "y" v <$> get)
                >> dependency ("y", v)
                >> pre <$> get

pvcg (Assign "z" (Fun (ReadRPMSymbols "y")))
    = \pre -> do
              liftIO $ putStrLn (show $ ReadRPMSymbols "y")
              let f = readRPMSymbols_
              let n = tyConName (typeRepTyCon (typeOf f))
              let v = if n == "->" then Just (F "y" f) else Nothing
              (put =<< insert "z" v <$> get)
                >> dependency ("z", v)
                >> pre <$> get

pvcg (Assign "h" (Fun (DeltaRPMSymbols  "z")))
    = \pre -> do
              liftIO $ putStrLn (show $ DeltaRPMSymbols "z")
              let f = deltaRPMSymbols_
              let n = tyConName (typeRepTyCon (typeOf f))
              let v = if n == "->" then Just (F "z" f) else Nothing
              (put =<< insert "h" v <$> get)
                >> dependency ("h", v)
                >> pre <$> get
-}
pvcg (Assign var (Fun name))
    = \pre -> do
              f <- flip (!) var <$> ask
              (put =<< insert var f <$> get)
                >> dependency (var, f)
                >> do
                   env <- get
                   liftIO $ putStrLn (show f)
                   liftIO $ putStrLn (show env)
                >> do
                   env <- get
                   liftIO $ pre env
                   --pre <$> get


pvcg (a :>>: b)
    = \pre -> pvcg b pre >>=
        \f -> do
                 pvcg a (\env -> resolve f env)

example = do
          let a = Assign "x" (Fun (ReadDeltaRPM "_"))
              b = Assign "y" (Fun (ApplyDeltaRPM "x"))
              c = Assign "z" (Fun (ReadRPMSymbols "y"))
              d = Assign "h" (Fun (DeltaRPMSymbols "z"))
              prog =  a :>>: (b :>>: (c :>>: d))
          (wp, env) <-runVCG (pvcg prog (resolve (preCondition empty))) metaFunctions empty
          let v = (env ! "h")
          --putStrLn (show v)
          putStrLn (show wp)


