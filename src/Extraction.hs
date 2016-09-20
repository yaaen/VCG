-----------------------------------------------------------------------------
--
-- Module      :  Extraction
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

module Extraction (
    Atom(..), CoqValue(..), as_list, Expr(..),
    Semantics (..), TypedExpression (..), TypedValue (..), DialectType (..),
    CoqProp (..), BaseType (..)
) where

import Prelude

data Atom =
   State Prelude.Char
 | File Prelude.Char
 | Symbol Prelude.Char Prelude.String
    deriving (Show)

type Undefined_symbol = ()

data CoqValue =
   ListOperations ([] Atom)
 | ListFilenames ([] Atom)
 | ListSymbols ([] Atom)
 | Product CoqValue CoqValue
 | Assemble ([] Atom)
    deriving (Show)

as_list :: CoqValue -> [] Atom
as_list value =
  case value of {
   ListOperations l -> l;
   ListFilenames l -> l;
   ListSymbols l -> l;
   Product l1 l2 -> (Prelude.++) (as_list l1) (as_list l2);
   Assemble l -> l}

data Expr =
   Statechart_elem Atom
 | Metadata_elem Atom
 | Object_elem Atom
 | Add_operation Expr
 | Rem_operation Expr
 | Delta ([] Expr)
 | Compile Expr
     deriving (Show)

data DialectType =
   Type_Base BaseType
 | Type_Delta BaseType
 | Type_Product BaseType
 | Type_Variant BaseType
     deriving (Show)

data BaseType =
   Type_Statechart
 | Type_Metadata
 | Type_Object
     deriving (Show)

data Semantics = Semantics (CoqValue, Expr, CoqValue) deriving (Show)
data TypedExpression = TypedExpression (CoqValue, Expr, DialectType) deriving (Show)
data TypedValue = TypedValue (CoqValue, DialectType) deriving (Show)

data CoqProp = CoqPTy TypedValue
             | CoqPEx TypedExpression
             | CoqPEv Semantics
            deriving (Show)
