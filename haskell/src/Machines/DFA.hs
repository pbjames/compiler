module Machines.DFA (
  dfa,
) where

import Machines.NFA (nfa)
import Machines.State (Regex, StateMachine)

dfa :: Regex Char -> StateMachine
dfa = processDfa . eReducer . nfa
 where
  processDfa = undefined

eReducer :: StateMachine -> StateMachine
eReducer = undefined
