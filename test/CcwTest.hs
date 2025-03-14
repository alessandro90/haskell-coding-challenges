module CcwTest where

import Test.Tasty
import Test.Tasty.HUnit

test1 :: TestTree
test1 = testCase "Should-be-2" $ (2 :: Int) `compare` 2 @?= EQ

test2 :: TestTree
test2 = testCase "Should-be-3" $ (2 :: Int) `compare` 2 @?= EQ

ccwTests :: TestTree
ccwTests = testGroup "ccw tests" [test1, test2]
