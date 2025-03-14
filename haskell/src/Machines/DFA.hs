module Machines.DFA (
  dfa,
  removeEPaths,
) where

import Data.List (findIndex, nub, transpose)
import Machines.NFA (nfa)
import Machines.State (Regex, State (..), StateMachine)

dfa :: Regex Char -> StateMachine
dfa = processDfa . removeEPaths . nfa
 where
  processDfa = id

removeEPaths :: StateMachine -> StateMachine
removeEPaths stm = targetEPath stm targetIdx
 where
  targetIdx = findIndex (\(State _ _ ys) -> not $ null ys) stm

targetEPath :: StateMachine -> Maybe Int -> StateMachine
targetEPath stm (Just i) = removeEPaths processStm
 where
  filterCyclicEState :: Int -> State -> State
  filterCyclicEState idx (State st xs ys) = State st xs (filter (/= idx) ys)
  filterNonEmptyEdges :: [Int] -> [Int]
  filterNonEmptyEdges e = if all (== head e) e then [-1] else filter (/= -1) e
  collapseEdges :: [[[Int]]] -> [[Int]]
  collapseEdges = map (foldr1 (++)) . transpose
  linkIndexesToState :: State -> [[Int]] -> State
  linkIndexesToState (State st xs _) zs = State st (filterNonEmptyEdges . nub <$> zipWith (++) xs zs) []
  linkedHeadState = linkIndexesToState selectedState $ collapseEdges $ reduceEEdges stm selectedState []
  selectedState = filterCyclicEState i $ stm !! i
  processStm = [if pred x == i then linkedHeadState else stm !! pred x | x <- [1 .. length stm]]
targetEPath stm Nothing = stm

reduceEEdges :: StateMachine -> State -> [Int] -> [[[Int]]]
reduceEEdges stm (State _ xs ys) vs = xs : map newCall newYs
 where
  newCall i = concat $ reduceEEdges stm (stm !! i) (i : vs)
  newYs = filter (`notElem` vs) ys
