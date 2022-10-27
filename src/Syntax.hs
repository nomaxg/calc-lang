module Syntax
  ( module Syntax
  ) where

type Name = String

data Expr
  = Float Double
  | Var String
  | BinOp Op Expr Expr
  | Assign String Expr
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  | Mod
  deriving (Eq, Ord, Show)

defaultExpr :: Expr
defaultExpr = Var "default"
