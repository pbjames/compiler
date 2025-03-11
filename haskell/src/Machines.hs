{-# LANGUAGE ImportQualifiedPost #-}

module Machines (
  StateType (..),
  Regex (..),
  State (..),
  singleton,
  stmRange,
  nfa,
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
  = State StateType [Int]
  | E StateType [Int]
  deriving (Eq, Show)
type StateMachine = [State]
type STMOp = StateMachine -> StateMachine -> StateMachine

stype :: State -> StateType
stype (State st _) = st
stype (E st _) = st

stmRange :: [Int]
stmRange = [0 .. 25]

singleton :: Char -> StateMachine
singleton c =
  [ startState
  , endState
  ]
 where
  startState = State Initial [if x == (ord c - 65) then 1 else -1 | x <- [0 .. 25]]
  endState = State Accept [-1 | _ <- stmRange]

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
  replaceIndexes (State st v) = State st (map (mappingsForT Map.!) v)
  replaceIndexes (E st v) = E st (map (mappingsForT Map.!) v)
  mappingsForT = Map.fromList $ (-1, -1) : zip [0 .. length t - 1] [length s ..]

juxtapose :: STMOp
juxtapose s t = map (linkAcceptingToIdxs correctInitStates Normal) s ++ map removeInitial t
 where
  correctInitStates = map (+ length s) $ initialStates t

starify :: StateMachine -> StateMachine
starify s = moveInitial $ map (linkAcceptingToIdxs (initialStates s) Accept) s
 where
  moveInitial t = E InitialAccepting correctInitStates : map (updateState . removeInitial) t
  correctInitStates = map (+ 1) $ initialStates s
  updateState (State st vs) = State st $ map (\x -> if x == -1 then -1 else x + 1) vs
  updateState (E st vs) = E st $ map (\x -> if x == -1 then -1 else x + 1) vs

linkAcceptingToIdxs :: [Int] -> StateType -> State -> State
linkAcceptingToIdxs xs s (State Accept _) = E s xs
linkAcceptingToIdxs _ _ s = s

removeInitial :: State -> State
removeInitial (State InitialAccepting v) = State Accept v
removeInitial (State Initial v) = State Normal v
removeInitial (E InitialAccepting v) = E Accept v
removeInitial (E Initial v) = E Normal v
removeInitial s = s

initialStates :: StateMachine -> [Int]
initialStates = findIndices (isInitialState . stype)
 where
  isInitialState s = s == Initial || s == InitialAccepting
