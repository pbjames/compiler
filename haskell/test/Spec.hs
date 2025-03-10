import Exercises.SLI (BinaryOperator (..), Expr (..), Statement (..), interp, maxArgs)
import Machines (Regex (..), State (..), StateType (..), singleton, stmRange)
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

exampleRegex :: Regex Char
exampleRegex = And (Or (Value 'a') (Value 'b')) (Star (Value 'c'))

generateState :: Char -> State
generateState c = State Initial [if x == charCode then 1 else -1 | x <- stmRange]
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
    ]
