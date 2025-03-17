module Unit.DFA (dfaTests) where

import Machines.DFA (dfa, removeEPaths)
import Machines.State (Regex (..), State (..), StateType (..), noEdges, stmRange)
import Test.HUnit (Test (..), assertEqual)

import Data.Char (ord)

exampleRegexA :: Regex Char
exampleRegexA = Value 'A'

exampleRegexC :: Regex Char
exampleRegexC = Value 'C'

exampleStarRegexC :: Regex Char
exampleStarRegexC = Star $ Value 'C'

exampleRegexB :: Regex Char
exampleRegexB = Value 'B'

exampleRegexAOrB :: Regex Char
exampleRegexAOrB = Or exampleRegexA exampleRegexB

exampleRegexAAndB :: Regex Char
exampleRegexAAndB = And exampleRegexA exampleRegexB

exampleRegexAABStar :: Regex Char
exampleRegexAABStar = Star $ And (And exampleRegexA exampleRegexA) exampleRegexB

exampleRegexDStarStar :: Regex Char
exampleRegexDStarStar = Star . Star $ Value 'D'

exampleRegexComplete :: Regex Char
exampleRegexComplete = And exampleRegexAOrB exampleStarRegexC

-- ((ab|ac)*(abb)*)*
exampleRegexNightmarish :: Regex Char
exampleRegexNightmarish =
  Star $
    And
      (Star $ Or (And exampleRegexA exampleRegexB) (And exampleRegexA exampleRegexC))
      exampleRegexAABStar

computeState :: Char -> StateType -> Int -> ([Int] -> State)
computeState c st idx = State st [if x == charCode then [idx] else [-1] | x <- stmRange]
 where
  charCode = ord c - 65

computeEState :: StateType -> [Int] -> State
computeEState st = State st noEdges

acceptingState :: State
acceptingState = State Accept noEdges []

dfaTests :: [Test]
dfaTests =
  [ -- TestCase $
    --   assertEqual
    --     "dfa-ereducer-1"
    --     [ State InitialAccepting ([[4], [4], [4]] ++ [[-1] | _ <- [1 .. 23]]) []
    --     , computeState 'A' Normal 4 []
    --     , computeState 'B' Normal 4 []
    --     , computeState 'C' Normal 4 []
    --     , acceptingState
    --     ]
    --     ( removeEPaths
    --         [ computeEState InitialAccepting [1, 2, 3]
    --         , computeState 'A' Normal 4 []
    --         , computeState 'B' Normal 4 []
    --         , computeState 'C' Normal 4 []
    --         , acceptingState
    --         ]
    --     )
    TestCase $
      assertEqual
        "dfa-ereducer-2"
        [ State InitialAccepting ([[4], [4], [4], [4]] ++ [[-1] | _ <- [1 .. 22]]) []
        , computeState 'A' Normal 4 []
        , computeState 'B' Normal 4 []
        , computeState 'C' Normal 4 [5]
        , acceptingState
        , computeState 'D' Normal 4 []
        ]
        ( removeEPaths
            [ computeEState InitialAccepting [1, 2, 3]
            , computeState 'A' Normal 4 []
            , computeState 'B' Normal 4 []
            , computeState 'C' Normal 4 [5]
            , acceptingState
            , computeState 'D' Normal 4 []
            ]
        )
  ]

-- [State InitAC [[04],[04],[04],[04],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]] []
-- ,State Normal [[04],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]] []
-- ,State Normal [[-1],[04],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]] []
-- ,State Normal [[-1],[-1],[04],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]] [5]
-- ,State Accept [[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]] []
-- ,State Normal [[-1],[-1],[-1],[04],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]] []]

-- [State InitAC [[04],[04],[04],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]] []
-- ,State Normal [[04],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]] []
-- ,State Normal [[-1],[04],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]] []
-- ,State Normal [[-1],[-1],[04],[04],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]] []
-- ,State Accept [[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]] []
-- ,State Normal [[-1],[-1],[-1],[04],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1],[-1]] []]
