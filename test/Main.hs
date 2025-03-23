module Main (main) where

-- The tests can be run like so:
-- cabal test --test-options "-p Should-be-2"

import CcwTest (parseArgsTests)
import MiscTest (startsWithTests)
import Test.Tasty

allTests :: TestTree
allTests =
  testGroup
    "all-tests"
    [ startsWithTests,
      parseArgsTests
    ]

main :: IO ()
main = defaultMain allTests
