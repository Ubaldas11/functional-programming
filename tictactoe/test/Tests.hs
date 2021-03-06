import Test.Tasty
import Test.Tasty.HUnit
import Data.List
import Data.Ord

import MinimaxTests
import ValidatorTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [minimaxTests, validatorTests]