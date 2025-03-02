module Exercises.SLI (
  Statement (..),
  Expr (..),
  BinaryOperator (..),
  interp,
  maxArgs,
) where

-- INFO: Single line interpreter (skipped parsing and lexing)
--
import Data.Foldable (find)

type Identifier = String
type Var = (Identifier, Int)
data BinaryOperator = Plus | Minus | Multiply | Divide
data Expr
  = Id Identifier
  | Number Int
  | Op BinaryOperator Expr Expr
  | Eseq Statement Expr
data Statement
  = Compound Statement Statement
  | Assign Identifier Expr
  | Print [Expr]

nameInVar :: Identifier -> Var -> Bool
nameInVar name (vname, _) = name == vname

maxArgs :: Statement -> Int
maxArgs (Compound s t) = max (maxArgs s) (maxArgs t)
maxArgs (Assign _ (Eseq s _)) = maxArgs s
maxArgs (Assign _ _) = 0
maxArgs (Print exprs)
  | null exprs = 0
  | otherwise = max (length exprs) $ maximum $ maxArgs . intoSeq <$> exprs
 where
  intoSeq :: Expr -> Statement
  intoSeq (Eseq s _) = s
  intoSeq _ = Print []

interp :: Statement -> [Var]
interp = interpLine []

interpLine :: [Var] -> Statement -> [Var]
interpLine vs (Compound s t) = interpLine (interpLine vs s) t
interpLine vs (Print _) = vs -- INFO: Would use side effects
interpLine vs (Assign name expr) = update vs (name, interpEval vs expr)
 where
  update :: [Var] -> Var -> [Var]
  update ut (n, x) = (n, x) : filter (not . nameInVar n) ut

interpEval :: [Var] -> Expr -> Int
interpEval vs (Eseq s e) = interpEval (interpLine vs s) e
interpEval vs (Id name) = maybe (-999) snd $ find (nameInVar name) vs
interpEval _ (Number x) = x
interpEval vs (Op op ea eb) = convert op (interpEval vs ea) (interpEval vs eb)
 where
  convert :: BinaryOperator -> (Int -> Int -> Int)
  convert Plus = (+)
  convert Minus = (-)
  convert Divide = div
  convert Multiply = (*)
