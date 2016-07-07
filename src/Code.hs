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
 deriving (Eq)

instance Show Expr where
    show (Statechart_elem atom) = "statechart_elem( " ++ show atom  ++ " )"
    show (Metadata_elem atom) = "metadata_elem( "  ++ show atom  ++ " )"
    show (Object_elem atom) = "object_elem( " ++ show atom ++ " )"
    show (Add_operation expr) = "add_operation( "  ++ show expr ++ " )"
    show (Rem_operation expr) = "rem_operation( "  ++ show expr ++ " )"
    show (Delta expr) = "delta( "  ++ show expr  ++ " )"
    show (Compile expr) = "compile( "  ++ show expr  ++ " )"

infixr 1 :==>:

data Prop =
   Prop :==>: Prop
 | CoqTypedValue Value
 | CoqEvaluation Value Expr Value
 | CoqTypedExpression Value Expr
 deriving (Eq)

instance Show Prop where
    show (a :==>: b)
        = show a ++ " -> " ++ "\n" ++ show b
    show (CoqTypedValue (ListSymbols symbols))
        = "TypedValue (" ++ show symbols ++ ", Type_Base Type_Object)"
    show (CoqEvaluation (ListSymbols core) (Compile program) (ListSymbols value))
        = "Semantics" ++ "(" ++ show core ++ ", " ++ "compile( " ++ show program ++ "))"
             ++ " " ++ "(Assemble (" ++ show value ++ "))"
    show (CoqTypedExpression  (ListSymbols core) (Compile program))
        = "TypedExpression (" ++ show core ++ ", " ++ "compile (" ++ show program ++ "), "
             ++ " Type_Variant Type_Object)"

