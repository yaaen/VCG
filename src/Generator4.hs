{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  Generator4
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

module Generator4 ( example ) where


import Prelude hiding (lookup, filter, map)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import System.Process
import System.Exit
import Bifunctor
import Control.Applicative hiding (empty)
import Data.Map
import qualified Data.List as L
import Data.Maybe
import Functions
import System.IO.Unsafe
import Extraction
import Text.PrettyPrint hiding (empty)

#ifndef DELTA_RPM
#define DELTARPM "ecall-delta-1.0-1.drpm"
#endif

infix  4 :<:, :<=:, :>:, :>=:, :=:, :/=:
infixl 3 :/\:
infixl 2 :\/:
infixr 1 :=>:, :>>:
infix  0 :<=>:

-- meta-functions
type Var = String
data UninterFun = ReadDeltaRPM
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

data Term = Var String | Val UninterFun
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
              | PTy Term
              | PEx Term Term
              | PEv Term Term Term
              | Coq CoqProp

instance Show Formula where
    show (a :=>: b)     = show a ++ " -> " ++ "\n" ++ show b
    show (Coq coqprop)  = show coqprop
    show any            = "any"

data IValue = I (IO Value) | F String (Value -> IO Value) deriving (Show)

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
       Formula ->
       VCG Formula
pvcg (Assign "x" (Fun ReadDeltaRPM))
    = \pre -> do
              liftIO $ putStrLn (show $ ReadDeltaRPM)
              (put =<< insert "x" (I readDeltaRPM_) <$> get)
                *> dependency ("x", (I readDeltaRPM_))
                *> pure pre

pvcg (Assign "y" (Fun (ApplyDeltaRPM "x")))
    = \pre -> do
              liftIO $ putStrLn (show $ ApplyDeltaRPM "x")
              (put =<< insert "y" (F "x" applyDeltaRPM_) <$> get)
                >> return pre

pvcg (Assign "z" (Fun (ReadRPMSymbols "y")))
    = \pre -> do
              liftIO $ putStrLn (show $ ReadRPMSymbols "y")
              (put =<< insert "z" (F "y" readRPMSymbols_) <$> get)
                >> return pre

pvcg (Assign "h" (Fun (DeltaRPMSymbols  "z")))
    = \pre -> do
              liftIO $ putStrLn (show $ DeltaRPMSymbols "z")
              (put =<< insert "h" (F "z" deltaRPMSymbols_) <$> get)
                >> return pre

pvcg (a :>>: b) = \pre -> pvcg b pre >>= pvcg a

example = do
          let a = Assign "x" (Fun ReadDeltaRPM)
              b = Assign "y" (Fun (ApplyDeltaRPM "x"))
              c = Assign "z" (Fun (ReadRPMSymbols "y"))
              d = Assign "h" (Fun (DeltaRPMSymbols "z"))
              prog =  a :>>: (b :>>: (c :>>: d))
          (wp, env) <-runVCG (pvcg prog post) (empty)
          putStrLn (show wp)
          mapM (putStrLn . show) (toList env)


type Store = Map String (IO Value)
data P = MkP { f :: Formula, st :: Store } deriving (Show)

wp ::
    MetaExpr ->
    P ->
    Store ->
    IO P

wp (Assign "x" (Fun ReadDeltaRPM))
    = \p s -> do
              putStrLn $ "ReadDeltaRPM"
              return (MkP (f p) (insert "x" readDeltaRPM_  (st p)))

wp (Assign "y" (Fun (ApplyDeltaRPM "x")))
    = \p s -> do
              putStrLn $ "ApplyDeltaRPM"
              return (MkP (f p) (insert "y" (applyDeltaRPM_ =<< (s ! "x")) (st p)))

wp (Assign "z" (Fun (ReadRPMSymbols "y")))
    = \p s -> do
              putStrLn "ReadRPMSymbols"
              return (MkP (f p) (insert "z" (readRPMSymbols_ =<< (s ! "y")) (st p)))

wp (Assign "h" (Fun (DeltaRPMSymbols "z")))
    = \p s -> do
              putStrLn "DeltaRPMSymbols"
              return (MkP (f p) (insert "h" (deltaRPMSymbols_ =<< (s ! "z")) (st p)))

wp (a :>>: b) = \p s ->
                do
                let e1  = \p' -> \s' -> wp a p' s'
                    e2  = \p' -> \s' -> wp b p' s'
                p' <- e1 p s
                p'' <- ((\s' -> e2 p s') . st) p'
                pure (MkP (f p'') (st p''))


expr_eval_ ::
    MetaExpr ->
    Store ->
    PreCondStoreT Store IO Store
expr_eval_ (Assign "x" (Fun ReadDeltaRPM)) s
    =  return $ insert "x" readDeltaRPM_ s

pred_eval_ ::
    MetaExpr ->
    Formula ->
    Formula
pred_eval_ (Assign "x" (Fun ReadDeltaRPM))
    =  \p -> p

wp_ :: MetaExpr -> Formula -> PreCondStoreT Store IO Formula

wp_ (Assign "x" (Fun ReadDeltaRPM))
    =  \p -> return p

wp_ (a :>>: b)
    = \p -> getT >>= expr_eval_ a >>= expr_eval_ b >>= putT >> ($) wp_ b p >>= wp_ a

newtype PreCondStore store a
    = PreCondStore (store -> (store,a))

instance Monad (PreCondStore store) where
    return a = PreCondStore (\store -> (store,a))
    PreCondStore run >>= action = PreCondStore run'
        where run' st = let (st', a) = run st
                            PreCondStore run'' = action a
                        in run'' st'

instance  Functor (PreCondStore store) where
   fmap f x = x >>= (pure . f)

instance Applicative (PreCondStore store) where
    pure   = return
    m *> k = m >>= \ _ -> k
    (<*>)  = ap

getStore :: PreCondStore store store
getStore = PreCondStore (\store -> (store, store))

putStore :: store -> PreCondStore store ()
putStore new = PreCondStore (\_ -> (new,()))

{-wp__ :: MetaExpr -> Formula -> PreCondStore Store Formula

wp__ (a :>>: b) = \p -> do
                        expr_eval_ b <$> expr_eval_ a <$> getStore >>= putStore
                        pred_eval_ a <$> pure (pred_eval_ b p)
-}

newtype PreCondStoreT store m a
    = PreCondStoreT (store -> m (store, a))

instance MonadTrans (PreCondStoreT store) where
    lift m = PreCondStoreT (\s -> do a <- m
                                     return (s,a))

instance Monad m => Monad (PreCondStoreT store m) where
    return a = PreCondStoreT (\store -> return (store,a))
    PreCondStoreT m >>= k
        = PreCondStoreT ( \s -> do
                                (s', a) <- m s
                                let PreCondStoreT m' = k a
                                m' s' )
    fail s = PreCondStoreT (\_ -> fail s)

instance  (Monad m) => Functor (PreCondStoreT store m) where
   fmap f x = do
              x >>= (pure . f)

instance (Monad m) => Applicative (PreCondStoreT store m) where
    pure   = return
    m *> k = m >>= \ _ -> k
    (<*>)  = ap


getT :: Monad m => PreCondStoreT s m s
getT = PreCondStoreT (\s -> return (s, s))

putT :: Monad m => s -> PreCondStoreT s m ()
putT s = PreCondStoreT (\_ -> return (s, ()))

evalPreCondStoreT :: Monad m => PreCondStoreT s m a -> s -> m a
evalPreCondStoreT (PreCondStoreT m) store
    = do
      (s', a) <- m store
      return a

test ::  PreCondStoreT Int IO Int
test = do
       lift $ putStrLn "OLA"
       return 0

testEval = evalPreCondStoreT test 0


post = Forall "h"
           (PEv (Val ProductCore) (Val (ComputeDelta "h")) (Var "h"))
                :=>: (PEx (Val ProductCore) (Val (ComputeDelta "h")))
                    :=>: (PTy (Var "h"))

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

header = text "Add LoadPath \".\" as Top." $+$
         text "Require Import Top.Semantics2." $+$
         text "Require Import List."

tactic = text "Ltac my_tauto :=" $+$
         nest 2 (text "repeat match goal with") $+$
         nest 12 (text "| [ H : _ = _  |- False ] => discriminate H") $+$
         nest 12 (text "| [ H : In _ _ |- False ] => apply in_inv in H; intuition") $+$
         nest 9 (text "end.")

lemma  = text "Lemma delta :"

proof = text "Proof." $+$
        nest 2 (text "intros H1 H2.") $+$
        nest 2 (text "assert (DefinedSymbols (symbols (Type_Delta Type_Object ("t"::nil))))") $+$
        nest 4 (text "by (unfold symbols; constructor; apply not_in_cons; intuition; my_tauto).") $+$
        nest 2 (text "eauto using TypeLemma.") $+$
        text "Qed."


example_ = do
           let a = Assign "x" (Fun ReadDeltaRPM)
               b = Assign "y" (Fun (ApplyDeltaRPM "x"))
               c = Assign "z" (Fun (ReadRPMSymbols "y"))
               d = Assign "h" (Fun (DeltaRPMSymbols "z"))
               prog =  a :>>: (b :>>: (c :>>: d))
           p <- wp prog (MkP TRUE empty) (empty)
           putStrLn (show ((st p) ! "h"))
           putStrLn (show (f p))
           f <- eval post (st p)

           let vcString = show (header $+$ tactic $+$ lemma $+$ text (show f ++ ".")  $+$ footer)

           writeFile "vc.v" vcString
           vcString' <- readFile "vc.v"
           putStrLn vcString'

           let cmd = "\"coqc\"  -q  -R \".\" Top -I \".\"  vc.v"
           (success, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) ""
           putStrLn $ show (success, stdout, stderr)



