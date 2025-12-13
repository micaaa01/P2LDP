module AST where

-- Operaciones binarias
data BinOp
  = Add | Sub | Mul | Div
  | And | Or
  | Equal | NotEqual
  | Less | Greater | LessEq | GreaterEq
  deriving (Eq, Show)

--Operaciones unarias
data UnOp
  = Neg        -- -e
  | Not        -- !e
  deriving (Eq, Show)

--Expresiones
data Expr
  = EVar String
  | ELam String Expr
  | EApp Expr Expr
  | ELet String Expr Expr
  | ENum Int
  | EBool Bool
  | EBinOp BinOp Expr Expr
  | EUnOp UnOp Expr
  deriving (Eq, Show)