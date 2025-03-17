module MiscTest where

import Misc (startsWith, startsWithItem)
import Test.Tasty
import Test.Tasty.HUnit

startsWithEmptyPrefixTest :: TestTree
startsWithEmptyPrefixTest =
  testCase "misc-starts-with-empty-prefix" $ do
    startsWith "" "any string" @? "Should be true"

startsWithEmptyStringTest :: TestTree
startsWithEmptyStringTest =
  testCase "misc-starts-with-empty-string" $ do
    not (startsWith "--" "") @? "Should be false"

startsWithNonMatchingTest :: TestTree
startsWithNonMatchingTest =
  testCase "misc-starts-with-non-matching-prefix" $ do
    not (startsWith "--" "-some-text") @? "Should not match"

startsWithMatchingTest :: TestTree
startsWithMatchingTest =
  testCase "misc-starts-with-matching-prefix" $ do
    startsWith "--" "--some-text" @? "Should match"

startsWithItemEmptyList :: TestTree
startsWithItemEmptyList =
  testCase "starts-with-item-empty-list" $ do
    not (startsWithItem (0 :: Int) []) @? "Should be false"

startsWithItemMatchList :: TestTree
startsWithItemMatchList =
  testCase "starts-with-item-match-list" $ do
    startsWithItem (0 :: Int) [0, 1] @? "Should be true"

startsWithItemNonMatchList :: TestTree
startsWithItemNonMatchList =
  testCase "starts-with-item-non-match-list" $ do
    not (startsWithItem (0 :: Int) [2, 1]) @? "Should be false"

startsWithTests :: TestTree
startsWithTests =
  testGroup
    "starts-with"
    [ startsWithEmptyPrefixTest,
      startsWithEmptyStringTest,
      startsWithNonMatchingTest,
      startsWithMatchingTest,
      startsWithItemEmptyList,
      startsWithItemMatchList,
      startsWithItemNonMatchList
    ]
