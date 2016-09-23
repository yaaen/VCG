{-# LANGUAGE DeriveGeneric #-}
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
import Text.PrettyPrint
import Generics.Deriving.Base (Generic)
import Generics.Deriving.Show (GShow, gshow)

data Atom =
   State Prelude.Char
 | File Prelude.Char
 | Symbol Prelude.Char Prelude.String
    deriving (Generic)

instance Show Atom where
    show (Symbol s str) = render $
                            (text "Symbol") <+>
                            (doubleQuotes (text [s])) <+>
                            (doubleQuotes (text str))
    show other          = gshow other

type Undefined_symbol = ()

data CoqValue =
   ListOperations ([] Atom)
 | ListFilenames ([] Atom)
 | ListSymbols ([] Atom)
 | Product CoqValue CoqValue
 | Assemble ([] Atom)
    deriving (Generic)

instance GShow CoqValue
instance Show CoqValue where
  show (ListSymbols []) = render $ (text "ListSymbols") <+> text "nil"
  show (ListSymbols ss) = render $ (text "ListSymbols") <+>
                            parens (foldl1 (<^>) (map (text .show) ss) <^> (text "nil"))
  show (Assemble ss)    = render $ (text "Assemble") <+>
                            parens (foldl1 (<^>) (map (text .show) ss) <^> (text "nil"))
  show other            = gshow other

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
 | Delta [Expr]
 | Compile Expr
    deriving (Generic)

instance GShow Expr
instance GShow Atom
instance Show Expr where
    show (Compile e)       =  render $ (text "Compile") <+> parens (text (show e))
    show (Delta d)         =  render $ (text "Delta") <+>
                                parens (foldl1 (<^>) (map (text .show) d) <^> (text "nil"))
    show (Add_operation e) = render $ (text "Add_operation") <+> parens (text (show e))
    show (Object_elem o)   = render $ (text "Object_elem") <+> parens (text (show o))
    show expr              = render $ parens $ text $ gshow expr

x <^> y = x <> text "::" <> y

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

data Semantics = Semantics (CoqValue, Expr, CoqValue)
instance Show Semantics where
    show (Semantics (c, e, v)) = render $
                                 (text "Semantics") <+>  text (show (c,e)) <+>
                                 parens (text (show v))

data TypedExpression = TypedExpression (CoqValue, Expr, DialectType) deriving (Show)
data TypedValue = TypedValue (CoqValue, DialectType) deriving (Show)

data CoqProp = CoqPTy TypedValue
             | CoqPEx TypedExpression
             | CoqPEv Semantics

instance Show CoqProp where
    show (CoqPTy pty) = show pty
    show (CoqPEx pex) = show pex
    show (CoqPEv pev) = show pev

