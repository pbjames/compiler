import Test.HUnit (runTestTTAndExit)
import Unit.NFA (nfaTests)

main :: IO ()
main = runTestTTAndExit nfaTests
