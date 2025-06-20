{-# LANGUAGE BinaryLiterals #-}

module MiscTest (miscTests) where

import qualified Data.ByteString as BS
import Misc (bsToBinaryString, splitStr, startsWith, startsWithItem, w8FoldBits, w8ToBinaryString)
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
  testCase "misc-starts-with-item-empty-list" $ do
    not (startsWithItem (0 :: Int) []) @? "Should be false"

startsWithItemMatchList :: TestTree
startsWithItemMatchList =
  testCase "misc-starts-with-item-match-list" $ do
    startsWithItem (0 :: Int) [0, 1] @? "Should be true"

startsWithItemNonMatchList :: TestTree
startsWithItemNonMatchList =
  testCase "misc-starts-with-item-non-match-list" $ do
    not (startsWithItem (0 :: Int) [2, 1]) @? "Should be false"

w8ToStringTest :: TestTree
w8ToStringTest =
  testCase "misc-w8-to-string" $ do
    let n = 0b11011101
        s = w8ToBinaryString n
    assertEqual "binary repr" "10111011" s

bsToStringTest :: TestTree
bsToStringTest =
  testCase "misc-bs-to-string" $ do
    let b0 = 0b11011100
        b1 = 0b10010101
        b2 = 0b10011100
        b3 = 0b11011001
        bs = b0 `BS.cons` b1 `BS.cons` b2 `BS.cons` b3 `BS.cons` BS.empty
        s = bsToBinaryString bs
    assertEqual "binary repr" "00111011101010010011100110011011" s

w8FoldBitsTest :: TestTree
w8FoldBitsTest =
  testCase "misc-w8-fold-bits" $ do
    let w8 = 0b11000111
    let str = w8FoldBits (\s bit -> s <> (if bit then "1" else "0")) "" w8
    assertEqual "should be equal" "11100011" str

splitStrTest :: TestTree
splitStrTest =
  testGroup
    "misc-split-str"
    [ testCase "misc-split-str-empty" $ do
        let s = splitStr ' ' ""
        assertEqual "expect ampty list" [] s,
      testCase "misc-split-str-no-sep" $ do
        let s = splitStr ' ' "1,2,3"
        assertEqual "expect ampty list" ["1,2,3"] s,
      testCase "misc-split-str-sep" $ do
        let s = splitStr ' ' "1 2,3"
        assertEqual "expect ampty list" ["1", "2,3"] s,
      testCase "misc-split-str-sep" $ do
        let s = splitStr ' ' "1   2,3"
        assertEqual "expect ampty list" ["1", "2,3"] s
    ]

miscTests :: TestTree
miscTests =
  testGroup
    "misc"
    [ startsWithEmptyPrefixTest,
      startsWithEmptyStringTest,
      startsWithNonMatchingTest,
      startsWithMatchingTest,
      startsWithItemEmptyList,
      startsWithItemMatchList,
      startsWithItemNonMatchList,
      w8ToStringTest,
      bsToStringTest,
      w8FoldBitsTest,
      splitStrTest
    ]
