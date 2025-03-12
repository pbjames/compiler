module Machines.State (
  StateType (..),
  Regex (..),
  State (..),
  singleton,
  stmRange,
  noEdges,
  mapOverValues,
  stype,
  STMOp,
  StateMachine,
) where

import Data.Char (ord)

data StateType = Accept | Initial | InitialAccepting | Normal deriving (Eq, Show)
data Regex c
  = Star (Regex c)
  | Or (Regex c) (Regex c)
  | And (Regex c) (Regex c)
  | Empty
  | Value c
  deriving (Show)
data State
  = State StateType [Int] [Int]
  deriving (Eq, Show)
type StateMachine = [State]
type STMOp = StateMachine -> StateMachine -> StateMachine

stype :: State -> StateType
stype (State st _ _) = st

mapOverValues :: (Int -> Int) -> State -> State
mapOverValues f (State st xs ys) = State st (map f xs) (map f ys)

stmRange :: [Int]
stmRange = [0 .. 25]

noEdges :: [Int]
noEdges = [-1 | _ <- stmRange]

singleton :: Char -> StateMachine
singleton c =
  [ startState
  , endState
  ]
 where
  startState = State Initial [if x == (ord c - 65) then 1 else -1 | x <- stmRange] []
  endState = State Accept noEdges []
