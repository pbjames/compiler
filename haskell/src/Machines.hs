module Machines (
  StateType (..),
  Regex (..),
) where

import Data.Char (ord)
import Data.Map (Map)

data StateType = Accept | Start | Normal
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
  endState = (Accept, [-1 | x <- [0 .. 25]])

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
 where
  connect :: StateMachine -> StateMachine -> StateMachine
  connect s t = update s t ++ t

  update :: StateMachine -> StateMachine -> StateMachine
  update = undefined
