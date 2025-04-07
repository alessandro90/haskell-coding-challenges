{-# LANGUAGE OverloadedStrings #-}

module HuffmanTest where

-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.PQueue.Prio.Min as PQ
import Huffman (countFreq)
import Test.Tasty
import Test.Tasty.HUnit

countFreqTest :: TestTree
countFreqTest =
  testGroup
    "huffman-count-freq"
    [ testCase "huffman-count-freq-empty" $ do
        let freqs = countFreq ""
        assertBool "Should be empty list" $ PQ.null freqs,
      testCase "huffman-count-freq-letters" $ do
        let freqs = countFreq "aaxababx b"
            expected =
              PQ.fromList
                [ (1, BSI.c2w ' '),
                  (2, BSI.c2w 'x'),
                  (3, BSI.c2w 'b'),
                  (4, BSI.c2w 'a')
                ]
        assertEqual "Should be an ordered list" expected freqs
    ]

huffmanTests :: TestTree
huffmanTests =
  testGroup
    "huffman"
    [countFreqTest]
