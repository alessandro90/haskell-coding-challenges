module Main (main) where

-- The tests can be run like so:
-- cabal test --test-options "-p Should-be-2"

import CcwTest (parseArgsTests)
import CutTest (cutTests)
import HuffmanTest (huffmanTests)
import JsonTest (jsonTests)
import MiscTest (miscTests)
import Test.Tasty

allTests :: TestTree
allTests =
  testGroup
    "all-tests"
    [ miscTests,
      parseArgsTests,
      jsonTests,
      huffmanTests,
      cutTests
    ]

main :: IO ()
main = defaultMain allTests
