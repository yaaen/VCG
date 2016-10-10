{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Generics.Deriving.Base (Generic)
import Generics.Deriving.Show (GShow, gshow)

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
              | PEv String
              | PEx String
              | PTy String
              | CoqPTy TypedValue
              | CoqPEx TypedExpression
              | CoqPEv Semantics
              deriving (Generic)

instance GShow Formula
instance GShow Semantics
instance GShow TypedExpression
instance GShow TypedValue


instance Show Formula where
    show (a :=>: b)     = show a ++ " -> " ++ "\n" ++ show b
    show (Forall x f)   = "Forall " ++ x ++ " . " ++ show f
    show (PEv x )       = "PEv " ++ show x
    show (PEx x )       = "PEx " ++ show x
    show (PTy x )       = "PTy " ++ show x
    show (CoqPEv p)     = show p
    show (CoqPEx p)     = show p
    show (CoqPTy p)     = show p
    show any            = gshow any


pre :: Formula
pre = Forall "h" (PEv "h") :=>: (PEx "h") :=>: (PTy "h")

compute :: Formula -> Env -> VCG Formula
compute f env
    = case env ! "h" of
        I (C v) -> compute_ f v
        F _ _   -> return f

compute_ (Forall "h" f) val
    = compute_ f val
compute_ (f_a :=>: f_b ) val
    = liftM2 ( (:=>:) ) (compute_ f_a val) (compute_ f_b val)
compute_ (PEv "h") val
    = do
      let ListSymbols syms = val
      let delta = Delta (L.map (\s -> Add_operation (Object_elem s) (Delta [])) (L.take 2 syms) )
          value = Assemble (L.take 2 syms)
      pure (CoqPEv (Semantics (delta, value)))
compute_ (PEx "h") val
    = do
      let ListSymbols syms = val
          core =  ListSymbols []
          delta = Delta (L.map (\s -> Add_operation (Object_elem s) (Delta [])) (L.take 2 syms))
          abst = L.map (\(Symbol s _) -> Flag s) (L.take 2 syms)
      pure (CoqPEx (TypedExpression (core, delta, Type_Delta Type_Object abst)))
compute_ (PTy "h") val
    = do
      liftIO $ putStrLn $ "====================\n COMPUTE WP Ty" ++
                show val  ++ " \n===================="
      return (PTy "h")
compute_ f val
    = do
      liftIO $ putStrLn $ "====================\n COMPUTE WP Ty" ++
                show f  ++ " \n===================="
      return f


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
       Formula ->
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
    = \p     -> do
                fun <- flip (!) var <$> ask
                (put =<< insert var fun <$> get)
                  >> dependency (var, fun)
                  >> do
                     env <- get
                     liftIO $ putStrLn (show fun)
                     liftIO $ putStrLn (show env)
                  >> do
                     compute p =<< get
                     --pre <$> get


pvcg (a :>>: b)
   -- = \p -> pvcg b p >>= \p' -> pvcg a p'
    = \p -> pvcg a =<< pvcg b p

example = do
          let a = Assign "x" (Fun (ReadDeltaRPM "_"))
              b = Assign "y" (Fun (ApplyDeltaRPM "x"))
              c = Assign "z" (Fun (ReadRPMSymbols "y"))
              d = Assign "h" (Fun (DeltaRPMSymbols "z"))
              prog =  a :>>: (b :>>: (c :>>: d))
          (wp, env) <-runVCG (pvcg prog pre) metaFunctions empty
          let v = (env ! "h")
          --putStrLn (show v)
          putStrLn (show wp)


