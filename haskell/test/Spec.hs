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

exampleStarRegexC :: Regex Char
exampleStarRegexC = Star $ Value 'C'

exampleRegexB :: Regex Char
exampleRegexB = Value 'B'

exampleRegexAOrB :: Regex Char
exampleRegexAOrB = Or exampleRegexA exampleRegexB

exampleRegexAAndB :: Regex Char
exampleRegexAAndB = And exampleRegexA exampleRegexB

exampleRegexComplete :: Regex Char
exampleRegexComplete = And exampleRegexAOrB exampleStarRegexC

computeState :: Char -> StateType -> Int -> State
computeState c st idx = State st [if x == charCode then idx else -1 | x <- stmRange] []
 where
  charCode = ord c - 65

computeEState :: StateType -> Int -> State
computeEState st idx = State st noEdges [idx]

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
          , computeEState Normal 2
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
          [ computeEState InitialAccepting 1
          , computeState 'C' Normal 2
          , computeEState Accept 1
          ]
          (nfa exampleStarRegexC)
    , TestCase $
        assertEqual
          "nfa-1"
          [ computeState 'A' Initial 1
          , computeEState Normal 4
          , computeState 'B' Initial 3
          , computeEState Normal 4
          , computeEState Accept 5
          , computeState 'C' Normal 6
          , computeEState Accept 5
          ]
          (nfa exampleRegexComplete)
    ]
