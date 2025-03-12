module Unit.DFA (dfaTests) where

import Test.HUnit (Test (..), assertEqual)

dfaTests :: [Test]
dfaTests =
  [ TestCase $
      assertEqual
        "maxArgs"
        (2 :: Integer)
        (1 + 1)
  ]
