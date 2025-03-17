module Main (main) where

-- The tests can be run like so:
-- cabal test --test-options "-p Should-be-2"

-- import CcwTest (ccwTests)
import MiscTest (startsWithTests)
import Test.Tasty

allTests :: TestTree
allTests = testGroup "all-tests" [startsWithTests]

main :: IO ()
main = defaultMain allTests
