import Lib (BinaryOperator (..), Expr (..), Statement (..), maxArgs)
import Test.HUnit (Test (..), assertEqual, runTestTTAndExit)

main :: IO ()
main = runTestTTAndExit tests

exampleForMaxArgs :: Statement
exampleForMaxArgs =
  Compound
    (Assign "a" (Op Plus (Number 3) (Number 5)))
    (Print [Number 3, Id "a"])

tests :: Test
tests =
  TestList
    [ TestCase $
        assertEqual
          "maxArgs-1"
          2
          (maxArgs exampleForMaxArgs)
    ]
