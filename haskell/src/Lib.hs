module Lib (
  Statement (..),
  Expr (..),
  BinaryOperator (..),
  maxArgs,
) where

type Identifier = String
data BinaryOperator = Plus | Minus | Times | Divide
data Expr
  = Id Identifier
  | Number Int
  | Op BinaryOperator Expr Expr
  | Eseq Statement Expr
data Statement
  = Compound Statement Statement
  | Assign Identifier Expr
  | Print [Expr]

maxArgs :: Statement -> Int
maxArgs (Compound s t) = max (maxArgs s) (maxArgs t)
maxArgs (Print exprs) = length exprs
maxArgs (Assign _ _) = 0
