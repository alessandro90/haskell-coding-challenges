module CcwTest where

import Test.HUnit

test1 :: Test
test1 = TestCase (assertEqual "Should be 2" 2 2)

ccwTests :: Test
ccwTests = TestList [TestLabel "test1" test1]
