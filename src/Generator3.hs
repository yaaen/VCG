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
                | ComputeDelta Var
                | ProductCore
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

post = \env ->
        Forall "h"
           (Coq (CoqPEv (subst_ev env "h")))
                :=>: (Coq (CoqPEx (subst_ex env "h")))
                    :=>: (Coq (CoqPTy (subst_ty env "h")))

subst_ev env "h"
    =  Semantics (ListSymbols [], Delta [], ListSymbols [])
subst_ex env "h"
    =  TypedExpression (ListSymbols [], Delta [], Type_Delta Type_Object)
subst_ty env "h"
    =  TypedValue (ListSymbols [], Type_Delta Type_Object)


data IValue = I (IO Value) |
              F String (Value -> IO Value)
            deriving (Show, Typeable)

instance Show (IO Value) where
  show v = show $ unsafePerformIO v

instance Show (Value -> IO Value) where
  show v = "function"

type Env  = Map String (IValue)
type VCG a = StateT Env (IO) a
runVCG :: VCG a -> Env ->  IO (a, Env)
runVCG g pre = runStateT g pre

dependency
    :: (String, IValue) ->
       VCG ()
dependency (var, F _ fun)
    = return ()
dependency (var, I val)
    = filter somearg <$> get >>=
        \case map | size map == 1 ->
                       pure (bimap id (\(F var f) -> I (val >>= f)) (dep map)) >>=
                            \kv -> (put =<< (uncurry insert) kv <$> get)
                                 *> dependency kv
                  | otherwise -> return ()
      where
        somearg value = case value of { I _ -> False; F var' _ -> var == var' }
        dep map = (head (toList map))


pvcg
    :: MetaExpr ->
       (Env -> Formula) ->
       VCG Formula
pvcg (Assign "x" (Fun (ReadDeltaRPM "_")))
    = \pre -> do
              liftIO $ putStrLn (show $ ReadDeltaRPM "_")
              let v = readDeltaRPM_
              let n = tyConName (typeRepTyCon (typeOf v))
              let m = if n == "->" then Just (I v) else Nothing
              (put =<< insert "x" (I v) <$> get)
                >> dependency ("x", I v)
                >> pre <$> get

pvcg (Assign "y" (Fun (ApplyDeltaRPM "x")))
    = \pre -> do
              liftIO $ putStrLn (show $ ApplyDeltaRPM "x")
              let f = applyDeltaRPM_
              let n = tyConName (typeRepTyCon (typeOf f))
              let v = if n == "->" then Just (F "x" f) else Nothing
              (put =<< insert "y" (F "x" f) <$> get)
                >> dependency ("y", (F "x" f))
                >> pre <$> get

pvcg (Assign "z" (Fun (ReadRPMSymbols "y")))
    = \pre -> do
              liftIO $ putStrLn (show $ ReadRPMSymbols "y")
              (put =<< insert "z" (F "y" readRPMSymbols_) <$> get)
                >> dependency ("z", (F "y" readRPMSymbols_))
                >> pre <$> get

pvcg (Assign "h" (Fun (DeltaRPMSymbols  "z")))
    = \pre -> do
              liftIO $ putStrLn (show $ DeltaRPMSymbols "z")
              (put =<< insert "h" (F "z" deltaRPMSymbols_) <$> get)
                >> dependency ("h", (F "z" deltaRPMSymbols_))
                >> pre <$> get

pvcg (a :>>: b) = \pre -> pvcg b pre >>= \pre' -> pvcg a (\env -> pre')

example = do
          let a = Assign "x" (Fun (ReadDeltaRPM "_"))
              b = Assign "y" (Fun (ApplyDeltaRPM "x"))
              c = Assign "z" (Fun (ReadRPMSymbols "y"))
              d = Assign "h" (Fun (DeltaRPMSymbols "z"))
              prog =  a :>>: (b :>>: (c :>>: d))
          (wp, env) <-runVCG (pvcg prog post) (empty)
          --putStrLn (show wp)
          --mapM (putStrLn . show) ((keys env))
          let I v = (env ! "h")
          putStrLn (show v)

