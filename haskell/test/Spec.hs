import Exercises.SLI (BinaryOperator (..), Expr (..), Statement (..), interp, maxArgs)
import Machines (Regex (..), State (..), StateType (..), char2STM, nfa, singleton, stmRange)
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

exampleRegex1 :: Regex Char
exampleRegex1 = And exampleRegexA exampleRegexB

exampleRegex2 :: Regex Char
exampleRegex2 = And (Or exampleRegexA exampleRegexB) exampleStarRegexC

generateState :: Char -> State
generateState c = State Initial [if x == charCode then 1 else -1 | x <- stmRange]
 where
  charCode = ord c - 65

generateStateWith :: Char -> StateType -> State
generateStateWith c st = State st [if x == charCode then 1 else -1 | x <- stmRange]
 where
  charCode = ord c - 65

acceptingState :: State
acceptingState = State Accept [-1 | x <- stmRange]

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
          [ generateState 'A'
          , acceptingState
          ]
          (singleton 'A')
    , TestCase $
        assertEqual
          "singleton-statemachine-2"
          [ generateState 'Z'
          , acceptingState
          ]
          (singleton 'Z')
    , TestCase $
        assertEqual
          "nfa-basic-1"
          [ generateState 'A'
          , acceptingState
          ]
          (nfa $ char2STM exampleRegexA)
    , TestCase $
        assertEqual
          "nfa-basic-2"
          [ generateState 'B'
          , acceptingState
          ]
          (nfa $ char2STM exampleRegexB)
    , TestCase $
        assertEqual
          "nfa-1"
          [ generateState 'A'
          , E Normal [2]
          , generateState 'B'
          , acceptingState
          ]
          (nfa $ char2STM exampleRegex1)
    , TestCase $
        assertEqual
          "nfa-star-1"
          [ E InitialAccepting [1]
          , generateStateWith 'C' Normal
          , E Accept [1]
          ]
          (nfa $ char2STM exampleStarRegexC)
    ]
