{-# LANGUAGE ImportQualifiedPost #-}

module Machines (
  StateType (..),
  Regex (..),
  State (..),
  singleton,
  stmRange,
  nfa,
  noEdges,
) where

import Data.Char (ord)
import Data.List (findIndices)
import Data.Map qualified as Map

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
  startState = State Initial [if x == (ord c - 65) then 1 else -1 | x <- [0 .. 25]] []
  endState = State Accept noEdges []

-- We're collapsing all the 'singleton' machines into one recursively
nfa :: Regex Char -> StateMachine
nfa = process . char2STM
 where
  process :: Regex StateMachine -> StateMachine
  process (Value s) = s
  process (And s t) = disjointMap (process s) (process t) juxtapose
  process (Or s t) = disjointMap (process s) (process t) (++)
  process (Star s) = starify $ process s
  process Empty = undefined

  char2STM :: Regex Char -> Regex StateMachine
  char2STM (Star r) = Star (char2STM r)
  char2STM (Or r s) = Or (char2STM r) (char2STM s)
  char2STM (And r s) = And (char2STM r) (char2STM s)
  char2STM (Value c) = Value $ singleton c
  char2STM Empty = Empty

disjointMap :: StateMachine -> StateMachine -> STMOp -> StateMachine
disjointMap s t f = f s (map replaceIndexes t)
 where
  replaceIndexes = mapOverValues (mappingsForT Map.!)
  mappingsForT = Map.fromList $ (-1, -1) : zip [0 .. length t - 1] [length s ..]

juxtapose :: STMOp
juxtapose s t = map (linkAcceptingToIdxs correctInitStates Normal) s ++ map removeInitial t
 where
  correctInitStates = map (+ length s) $ initialStates t

starify :: StateMachine -> StateMachine
starify s = moveInitial $ map (linkAcceptingToIdxs (initialStates s) Accept) s
 where
  moveInitial t = State InitialAccepting noEdges correctInitStates : map (updateState . removeInitial) t
  correctInitStates = map (+ 1) $ initialStates s
  updateState = mapOverValues (\x -> if x == -1 then -1 else x + 1)

linkAcceptingToIdxs :: [Int] -> StateType -> State -> State
-- WARN: Should we link initial accepting states?
linkAcceptingToIdxs ys s (State Accept xs zs) = State s xs $ zs ++ ys
linkAcceptingToIdxs _ _ s = s

removeInitial :: State -> State
removeInitial (State InitialAccepting xs vs) = State Accept xs vs
removeInitial (State Initial xs vs) = State Normal xs vs
removeInitial s = s

initialStates :: StateMachine -> [Int]
initialStates = findIndices (isInitialState . stype)
 where
  isInitialState s = s == Initial || s == InitialAccepting
