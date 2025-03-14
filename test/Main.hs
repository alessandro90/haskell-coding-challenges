module Main (main) where

import CcwTest (ccwTests)
import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT ccwTests
  pure ()
