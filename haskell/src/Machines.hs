module Machines (
  StateType (..),
  Regex (..),
  State (..),
  singleton,
  stmRange,
  nfa,
  char2STM,
) where

import Data.Char (ord)
import Data.List (findIndices)

data StateType = Accept | Initial | InitialAccepting | Normal deriving (Eq, Show)
data Regex c
  = Star (Regex c)
  | Or (Regex c) (Regex c)
  | And (Regex c) (Regex c)
  | Empty
  | Value c
data State
  = State {stype :: StateType, values :: [Int]}
  | E {stype :: StateType, targets :: [Int]}
  deriving (Eq, Show)
type StateMachine = [State]

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

char2STM :: Regex Char -> Regex StateMachine
char2STM (Star r) = char2STM r
char2STM (Or r s) = Or (char2STM r) (char2STM s)
char2STM (And r s) = And (char2STM r) (char2STM s)
char2STM (Value c) = Value $ singleton c
char2STM Empty = Empty

-- We're collapsing all the 'singleton' machines into one recursively
nfa :: Regex StateMachine -> StateMachine
nfa (Value s) = s
nfa (Or s t) = nfa s ++ nfa t
nfa (And s t) = nfa s `juxtapose` nfa t
nfa (Star s) = starify $ nfa s
nfa Empty = undefined

linkAcceptingToIdxs :: [Int] -> StateType -> State -> State
linkAcceptingToIdxs xs st (State Accept _) = E st xs
linkAcceptingToIdxs _ _ s = s

initialStates :: StateMachine -> [Int]
initialStates = findIndices ((== Initial) . stype)

starify :: StateMachine -> StateMachine
starify s = newInitial $ map (linkAcceptingToIdxs (initialStates s) Accept) s
 where
  newInitial t = E InitialAccepting (initialStates t) : map removeInitial t
  removeInitial (State Initial v) = State Normal v
  removeInitial t = t

juxtapose :: StateMachine -> StateMachine -> StateMachine
juxtapose s t = map (linkAcceptingToIdxs correctInitStates Normal) s ++ t
 where
  correctInitStates = map (+ length s) $ initialStates t
