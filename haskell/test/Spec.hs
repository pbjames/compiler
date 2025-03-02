import Exercises.SLI (BinaryOperator (..), Expr (..), Statement (..), interp, maxArgs)
import Test.HUnit (Test (..), assertEqual, runTestTTAndExit)

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
          -- INFO: Order matters apparently
          [("b", 80), ("a", 8)]
          (interp exampleSingleLine)
    ]
