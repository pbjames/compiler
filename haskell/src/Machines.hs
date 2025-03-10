module Machines (
  StateType (..),
  Regex (..),
) where

import Data.Char (ord)

data StateType = Accept | Start | Normal deriving (Eq)
data Regex c = Star (Regex c) | Or (Regex c) (Regex c) | And (Regex c) (Regex c) | Empty | Value c
type State = (StateType, [Int])
type StateMachine = [State]

singleton :: Char -> StateMachine
singleton c =
  [ startState
  , endState
  ]
 where
  startState = (Start, [if x == ord c then 1 else -1 | x <- [0 .. 25]])
  endState = (Accept, [-1 | x <- [0 .. 26]])

-- all letters + epsilon

almoSTM :: Regex Char -> Regex StateMachine
almoSTM (Star r) = almoSTM r
almoSTM (Or r s) = Or (almoSTM r) (almoSTM s)
almoSTM (And r s) = And (almoSTM r) (almoSTM s)
almoSTM (Value c) = Value $ singleton c
almoSTM Empty = Empty

-- We're collapsing all the 'singleton' machines into one recursively
nfa :: Regex StateMachine -> StateMachine
nfa (Value s) = s
nfa (Or s t) = nfa s ++ nfa t
nfa (And s t) = nfa s `connect` nfa t

connect :: StateMachine -> StateMachine -> StateMachine
connect a b = update a b ++ b

update :: StateMachine -> StateMachine -> StateMachine
update s t = map (linkStateToMachine t) (initialStates s)

initialStates :: StateMachine -> StateMachine
initialStates = filter ((== Start) . fst)

-- link state
linkStateToMachine :: StateMachine -> State -> State
linkStateToMachine t (Accept, vals) = undefined
linkStateToMachine t s@(Start, vals) = s
