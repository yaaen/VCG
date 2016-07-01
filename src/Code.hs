module Code where


data Atom =
   State String
 | File String
 | Symbol String String
 deriving (Show, Eq)

data Value =
   ListOperations [Atom]
 | ListFilenames [Atom]
 | ListSymbols [Atom]
 | Product Value Value
 | Assemble [Atom]
 | RPM String
 | Tuple (Value, Value)
 | Empty
 deriving (Show, Eq)

as_list :: Value -> [Atom]
as_list value =
  case value of
    ListOperations l -> l;
    ListFilenames l -> l;
    ListSymbols l -> l;
    Product l1 l2 ->  (as_list l1) ++ (as_list l2);
    Assemble l -> l

data Expr =
   Statechart_elem Atom
 | Metadata_elem Atom
 | Object_elem Atom
 | Add_operation Expr
 | Rem_operation Expr
 | Delta [Expr]
 | Compile Expr

data DialectType =
   Type_Base BaseType
 | Type_Delta BaseType
 | Type_Product BaseType
 | Type_Variant BaseType
data BaseType =
   Type_Statechart
 | Type_Metadata
 | Type_Object

