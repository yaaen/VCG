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
    BaseType (..)
) where

import Prelude
import Text.PrettyPrint
import Generics.Deriving.Base (Generic)
import Generics.Deriving.Show (GShow, gshow)

data Atom =
   State Prelude.Char
 | File Prelude.Char
 | Flag Prelude.Char
 | Symbol Prelude.Char Prelude.String
    deriving (Generic)

instance Show Atom where
    show (Symbol s str) = render $
                            (doubleQuotes (text [s])) <+>
                            (doubleQuotes (text str))
    show (Flag s      ) = render $
                            (doubleQuotes (text [s]))
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
  show (Assemble [])    = render $ (text "List") <+> text "nil"
  show (Assemble ss)    = render $ (text "List") <+>
                            parens (foldl1 (<^>)
                                        (map (\s -> parens (text "NM" <+> text (show s))) ss)
                                        <^> (text "nil"))
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
 | Add_operation Expr Expr
 | Rem_operation Expr Expr
 | Delta [Expr]
 | Compile Expr
    deriving (Generic)

instance GShow Expr
instance GShow Atom
instance Show Expr where
    show (Compile e)            =  render $ (text "Compile") <+> parens (text (show e))
    show (Delta [])             =  render $ (text "Delta") <+> (text "nil")
    show (Delta d)              =  render $ (text "Delta") <+>
                                    parens (foldl1 (<^>) (map (text .show) d) <^> (text "nil"))
    show (Add_operation e expr) = render $ (text "Add_operation") <+>
                                                parens (text (show e))
                                                    <+> parens (text (show expr))
    show (Object_elem o)        = render $ (text "Object_elem") <+> text (show o)
    show expr                   = render $ parens $ text $ gshow expr

x <^> y = x <> text "::" <> y

data DialectType =
   Type_Base BaseType
 | Type_Delta BaseType [Atom]
 | Type_Product BaseType
 | Type_Variant BaseType
     deriving (Generic)

instance GShow DialectType
instance GShow BaseType

instance Show DialectType where
    show (Type_Delta Type_Object []) = "Type_Delta Type_Object nil"
    show (Type_Delta Type_Object ss) = render $ (text "Type_Delta") <+> (text "Type_Object") <+>
                                        parens (foldl1 (<^>) (map (text .show) ss) <^> (text "nil"))
    show other = gshow other

data BaseType =
   Type_Statechart
 | Type_Metadata
 | Type_Object
     deriving (Show, Generic)

data Semantics = Semantics (Expr, CoqValue) deriving (Generic)

instance Show Semantics where
    show (Semantics (e, v)) = render $
                                 (text "Semantics") <+>  parens (text (show e)) <+>
                                     parens (text (show v))

data TypedExpression = TypedExpression (CoqValue, Expr, DialectType) deriving (Show, Generic)
data TypedValue = TypedValue (CoqValue, DialectType) deriving (Show, Generic)

{-data Prop = CoqPTy TypedValue
          | CoqPEx TypedExpression
          | CoqPEv Semantics

instance Show Prop where
    show (CoqPTy pty) = show pty
    show (CoqPEx pex) = show pex
    show (CoqPEv pev) = show pev
-}
