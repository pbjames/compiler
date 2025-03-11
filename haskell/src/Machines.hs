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
import qualified Data.Map as Map

data StateType = Accept | Initial | InitialAccepting | Normal deriving (Eq, Show)
data Regex c
  = Star (Regex c)
  | Or (Regex c) (Regex c)
  | And (Regex c) (Regex c)
  | Empty
  | Value c
  deriving (Show)
data State
  = State {stype :: StateType, values :: [Int]}
  | E {stype :: StateType, targets :: [Int]}
  deriving (Eq, Show)
type StateMachine = [State]
type SMOp = StateMachine -> StateMachine -> StateMachine

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
  process (Or s t) = disjointMap (process s) (process t) (++)
  process (And s t) = disjointMap (process s) (process t) juxtapose
  process (Star s) = starify $ process s
  process Empty = undefined

  char2STM :: Regex Char -> Regex StateMachine
  char2STM (Star r) = Star (char2STM r)
  char2STM (Or r s) = Or (char2STM r) (char2STM s)
  char2STM (And r s) = And (char2STM r) (char2STM s)
  char2STM (Value c) = Value $ singleton c
  char2STM Empty = Empty

disjointMap :: StateMachine -> StateMachine -> SMOp -> StateMachine
disjointMap s t f = f s (map replaceIndexes t)
 where
  replaceIndexes :: State -> State
  replaceIndexes (State st v) = State st (map (mappingsForT Map.!) v)
  replaceIndexes (E st v) = State st (map (mappingsForT Map.!) v)
  mappingsForT = Map.fromList $ (-1, -1) : zip [0 .. length t - 1] [length s ..]

juxtapose :: SMOp
juxtapose s t = map (linkAcceptingToIdxs correctInitStates Normal) s ++ map removeInitial t
 where
  correctInitStates = map (+ length s) $ initialStates t

starify :: StateMachine -> StateMachine
starify s = moveInitial $ map (linkAcceptingToIdxs correctInitStates Accept) s
 where
  moveInitial t = E InitialAccepting correctInitStates : map removeInitial t
  correctInitStates = map (+ 1) $ initialStates s

linkAcceptingToIdxs :: [Int] -> StateType -> State -> State
linkAcceptingToIdxs xs st (State Accept _) = E st xs
linkAcceptingToIdxs _ _ s = s

removeInitial :: State -> State
removeInitial (State Initial v) = State Normal v
removeInitial t = t

initialStates :: StateMachine -> [Int]
initialStates = findIndices ((== Initial) . stype)
