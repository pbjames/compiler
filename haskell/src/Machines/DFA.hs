module Machines.DFA (
  dfa,
) where

import Data.List (findIndex, nub, transpose)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Machines.NFA (nfa)
import Machines.State (Regex, STMOp, State (..), StateMachine, mapOverValues)

dfa :: Regex Char -> StateMachine
dfa = processDfa . removeEPaths . nfa
 where
  processDfa = id

removeEPaths :: StateMachine -> StateMachine
removeEPaths stm = targetEPath stm targetIdx
 where
  targetIdx = findIndex (\(State _ _ ys) -> not $ null ys) stm

targetEPath :: StateMachine -> Maybe Int -> StateMachine
targetEPath _ (Just i) | trace ("Got index: " ++ show i) False = undefined
targetEPath stm (Just i) = removeEPaths processStm
 where
  nonETransitionChildren :: State -> [[[Int]]]
  nonETransitionChildren (State _ xs []) = [xs]
  nonETransitionChildren (State _ _ ys) = map (concatMap nonETransitionChildren stm !!) ys
  filterCyclicEState :: Int -> State -> State
  filterCyclicEState idx (State st xs ys) = State st xs (filter (/= idx) ys)
  collapseEdges :: [[[Int]]] -> [[Int]]
  collapseEdges = foldr1 (++) . transpose
  linkIndexesToState :: State -> [[Int]] -> State
  linkIndexesToState (State st xs _) zs = State st (nub <$> zipWith (++) xs zs) []
  linkedHeadState = linkIndexesToState selectedState $ collapseEdges $ nonETransitionChildren selectedState
  selectedState = filterCyclicEState i $ stm !! i
  processStm = [if pred x == i then linkedHeadState else stm !! pred x | x <- [1 .. length stm]]
targetEPath stm Nothing = stm

disjointMap :: StateMachine -> StateMachine -> STMOp -> StateMachine
disjointMap s t f = f s (map replaceIndexes t)
 where
  replaceIndexes = mapOverValues (mappingsForT Map.!)
  mappingsForT = Map.fromList $ (-1, -1) : zip [0 .. length t - 1] [length s ..]
