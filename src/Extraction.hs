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
    Atom(..), Value(..), as_list, Expr(..), DialectType (..)
) where

import Prelude

data Atom =
   State Prelude.Char
 | File Prelude.Char
 | Symbol Prelude.Char Prelude.String
 deriving (Show)

type Undefined_symbol = ()

{-data Value =
   ListOperations ([] Atom)
 | ListFilenames ([] Atom)
 | ListSymbols ([] Atom)
 | Product Value Value
 | Assemble ([] Atom)
 deriving (Show)
-}

data Value =
   ListOperations [Atom]
 | ListFilenames [Atom]
 | ListSymbols [Atom]
 | Product Value Value
 | Assemble [Atom]
 | RPM String
 | Tuple (Value, Value)
 | Undefined
 deriving (Show)

as_list :: Value -> [] Atom
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

{-data Semantics =
   ADD_statechat Atom ([] Atom)
 | ADD_metadata Atom ([] Atom)
 | ADD_object Atom ([] Atom)
 | REMOVE_statechart Atom ([] Atom)
 | REMOVE_metadata Atom ([] Atom)
 | REMOVE_object Atom ([] Atom)
 | APPLY Value Expr ([] Expr) Value Value Semantics Semantics
 | COMPILE Value Expr Value Semantics
 deriving (Show)
-}

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

{-data TypedValue =
   TC_Statechart ([] Atom)
 | TC_Metadata ([] Atom)
 | TC_Symbols ([] Atom)
 | TC_Product Value Value BaseType TypedValue TypedValue
 | TC_Variant Value BaseType TypedValue
 deriving (Show)

data TypedExpression =
   TE_Statechart_Add Value Atom
 | TE_Statechart_Rem Value Atom
 | TE_Metadata_Add Value Atom
 | TE_Metadata_Rem Value Atom
 | TE_Symbols_Add Value Atom
 | TE_Symbols_Rem Value Atom
 | TE_Delta Value Expr ([] Expr) BaseType TypedExpression TypedExpression
 | TE_Compile Value Expr BaseType TypedExpression
 deriving (Show)
-}
