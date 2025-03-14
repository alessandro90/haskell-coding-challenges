module Main (main) where

-- The tests can be run like so:
-- cabal test --test-options "-p Should-be-2"

import CcwTest (ccwTests)
import Test.Tasty

main :: IO ()
main = defaultMain ccwTests
