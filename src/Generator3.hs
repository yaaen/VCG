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
import Text.PrettyPrint hiding (empty)
import System.Process
import System.Exit

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
              | PEv Var
              | PEx Var
              | PTy Var
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
      pure (CoqPEx (TypedExpression (delta, Type_Delta Type_Object abst)))
compute_ (PTy "h") val
    = do
      let ListSymbols syms = val
          core =  ListSymbols []
          value = Assemble (L.take 2 syms)
          abst = L.map (\(Symbol s _) -> Flag s) (L.take 2 syms)
      pure (CoqPTy (TypedValue (value, Type_Delta Type_Object abst)))

assertion :: Formula -> Env -> Doc
assertion f env
    = case env ! "h" of
        I (C v) -> let ListSymbols syms = v
                       abst = L.map (\(Symbol s _) -> Flag s) (L.take 2 syms)
                       value = text $ show $ Type_Delta Type_Object abst
                   in parens $
                        text ("DefinedSymbols") <+>
                        parens (text ("symbols") <+> parens (value))
        F _ _   -> text ""

data IValue = I (Value) |
              F String (Value -> Value)
            deriving (Typeable)

{-instance Show IValue where
    showsPrec _ (I v) = (("I" ++ " " ++ show v) ++)
    showsPrec _ (F v f) = (("F " ++ v ++ " " ++ show f) ++) -}

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

pvcg (Assign var (Fun name))
    = \p     -> do
                fun <- flip (!) var <$> ask
                (put =<< insert var fun <$> get)
                  >> dependency (var, fun)
                  >> get >>= compute p


pvcg (a :>>: b)
   = \p -> pvcg a =<< pvcg b p

header = text "Add LoadPath \".\" as Top." $+$
         text "Require Import Top.Semantics2." $+$
         text "Require Import List."

tactic = text "Ltac my_tauto :=" $+$
         nest 2 (text "repeat match goal with") $+$
         nest 12 (text "| [ H : _ = _  |- False ] => discriminate H") $+$
         nest 12 (text "| [ H : In _ _ |- False ] => apply in_inv in H; intuition") $+$
         nest 9 (text "end.")

lemma l = text "Lemma delta :" $+$
          text (show l) Text.PrettyPrint.<>
          text "."

proof value
        = text "Proof." $+$
          nest 2 (text "intros H1 H2.") $+$
          nest 2 ((text "assert") <+> value) $+$
          nest 4 (text "by (unfold symbols; constructor; apply not_in_cons; intuition; my_tauto).") $+$
          nest 2 (text "eauto using TypeLemma.") $+$
          text "Qed."


#ifndef DONT_DEFINE_DELTA
#define DELTARPM "ecall-delta-1.0-1.drpm"
#define DELTADIR "./data/"
#endif

example = do
          let a = Assign "x" (Fun (ReadDeltaRPM "_"))
              b = Assign "y" (Fun (ApplyDeltaRPM "x"))
              c = Assign "z" (Fun (ReadRPMSymbols "y"))
              d = Assign "h" (Fun (DeltaRPMSymbols "z"))
              prog =  a :>>: (b :>>: (c :>>: d))
          (wp, env) <-runVCG (pvcg prog pre) metaFunctions empty

          let source = render $
                       header $+$
                       tactic $+$
                       lemma wp $+$
                       proof (assertion wp env)

          writeFile (DELTADIR ++ "usr/local/lib/pcc/vc.v") source
          vcString' <- readFile "vc.v"
          putStrLn vcString'

          let cmd = "cd " ++ DELTADIR ++ "; cd usr/local/lib/pcc;" ++
                    "\"coqc\"  -q  -R \".\" Top -I \".\"  vc.v"
          (success, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) ""
          putStrLn $ show (cmd)
          putStrLn $ show (success)
          case success of
            ExitSuccess -> exitSuccess
            ExitFailure c -> putStrLn stderr >> exitFailure

