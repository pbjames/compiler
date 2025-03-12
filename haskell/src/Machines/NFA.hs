{-# LANGUAGE ImportQualifiedPost #-}

module Machines.NFA (
  nfa,
) where

import Data.List (findIndices)
import Data.Map qualified as Map

import Machines.State (
  Regex (..),
  STMOp,
  State (..),
  StateMachine,
  StateType (..),
  mapOverValues,
  noEdges,
  singleton,
  stype,
 )

-- WARN: I'm not sure if we handle the change from [Int] to [[Int]] properly but I'm
-- sure it doesn't matter for NFAs
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
