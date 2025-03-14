import Test.HUnit (runTestTTAndExit)
import Test.HUnit.Base (Test (TestList))
import Unit.DFA (dfaTests)
import Unit.NFA (nfaTests)
import Unit.SLI (sliTests)

main :: IO ()
main = runTestTTAndExit $ TestList $ sliTests ++ nfaTests ++ dfaTests
