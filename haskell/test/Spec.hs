import Exercises.SLI (BinaryOperator (..), Expr (..), Statement (..), interp, maxArgs)
import Machines (Regex (..), State (..), StateType (..), nfa, noEdges, singleton, stmRange)
import Test.HUnit (Test (..), assertEqual, runTestTTAndExit)

import Data.Char (ord)

main :: IO ()
main = runTestTTAndExit tests

exampleSingleLine :: Statement
exampleSingleLine =
  Compound
    (Assign "a" (Op Plus (Number 5) (Number 3)))
    ( Compound
        ( Assign
            "b"
            ( Eseq
                (Print [Id "a", Op Minus (Id "a") (Number 1)])
                (Op Multiply (Number 10) (Id "a"))
            )
        )
        (Print [Id "b"])
    )

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

computeState :: Char -> StateType -> Int -> State
computeState c st idx = State st [if x == charCode then idx else -1 | x <- stmRange] []
 where
  charCode = ord c - 65

computeEState :: StateType -> [Int] -> State
computeEState st = State st noEdges

acceptingState :: State
acceptingState = State Accept noEdges []

tests :: Test
tests =
  TestList
    [ TestCase $
        assertEqual
          "maxArgs"
          2
          (maxArgs exampleSingleLine)
    , TestCase $
        assertEqual
          "interp"
          [("b", 80), ("a", 8)]
          (interp exampleSingleLine)
    , TestCase $
        assertEqual
          "singleton-statemachine-1"
          [ computeState 'A' Initial 1
          , acceptingState
          ]
          (singleton 'A')
    , TestCase $
        assertEqual
          "singleton-statemachine-2"
          [ computeState 'Z' Initial 1
          , acceptingState
          ]
          (singleton 'Z')
    , TestCase $
        assertEqual
          "nfa-basic-1"
          [ computeState 'A' Initial 1
          , acceptingState
          ]
          (nfa exampleRegexA)
    , TestCase $
        assertEqual
          "nfa-basic-2"
          [ computeState 'B' Initial 1
          , acceptingState
          ]
          (nfa exampleRegexB)
    , TestCase $
        assertEqual
          "nfa-and-1"
          [ computeState 'A' Initial 1
          , computeEState Normal [2]
          , computeState 'B' Normal 3
          , acceptingState
          ]
          (nfa exampleRegexAAndB)
    , TestCase $
        assertEqual
          "nfa-or-1"
          [ computeState 'A' Initial 1
          , acceptingState
          , computeState 'B' Initial 3
          , acceptingState
          ]
          (nfa exampleRegexAOrB)
    , TestCase $
        assertEqual
          "nfa-star-1"
          [ computeEState InitialAccepting [1]
          , computeState 'C' Normal 2
          , computeEState Accept [1]
          ]
          (nfa exampleStarRegexC)
    , TestCase $
        assertEqual
          "nfa-1"
          [ computeState 'A' Initial 1
          , computeEState Normal [4]
          , computeState 'B' Initial 3
          , computeEState Normal [4]
          , computeEState Accept [5]
          , computeState 'C' Normal 6
          , computeEState Accept [5]
          ]
          (nfa exampleRegexComplete)
    , TestCase $
        assertEqual
          "nfa-2"
          [ computeEState InitialAccepting [1]
          , computeState 'A' Normal 2
          , computeEState Normal [3]
          , computeState 'A' Normal 4
          , computeEState Normal [5]
          , computeState 'B' Normal 6
          , computeEState Accept [1]
          ]
          (nfa exampleRegexAABStar)
    , TestCase $
        assertEqual
          "nfa-3"
          [ computeEState InitialAccepting [1]
          , computeEState Accept [2]
          , computeState 'D' Normal 3
          , computeEState Accept [2, 1]
          ]
          (nfa exampleRegexDStarStar)
    , TestCase $
        -- INFO: I checked this out manually and it appears to actually be correct
        assertEqual
          "nfa-4"
          [ State InitAC [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] [1]
          , State Accept [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] [2, 6]
          , State Normal [03, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] []
          , State Normal [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] [4]
          , State Normal [-1, 05, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] []
          , State Normal [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] [2, 6, 10]
          , State Normal [07, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] []
          , State Normal [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] [8]
          , State Normal [-1, -1, 09, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] []
          , State Normal [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] [2, 6, 10]
          , State Accept [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] [11, 1]
          , State Normal [12, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] []
          , State Normal [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] [13]
          , State Normal [14, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] []
          , State Normal [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] [15]
          , State Normal [-1, 16, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] []
          , State Accept [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] [11, 1]
          ]
          (nfa exampleRegexNightmarish)
    ]
