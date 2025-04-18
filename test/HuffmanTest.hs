{-# LANGUAGE OverloadedStrings #-}

module HuffmanTest where

import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w, w2c)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Min as PQ
import Data.Word (Word8)
import Huffman
  ( Bit (BitOff, BitOn),
    Tree (Leaf, Node),
    bfAllBytes,
    bfEmpty,
    bfPush,
    buildTree,
    codes,
    countFreq,
    leaf,
  )
import Misc (bsToBinaryString)
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
                [ leaf 1 ' ',
                  leaf 2 'x',
                  leaf 3 'b',
                  leaf 4 'a'
                ]
        assertEqual "Should be an ordered list" expected freqs
    ]

treeMergeTest :: TestTree
treeMergeTest =
  testGroup
    "huffman-tree-merge"
    [ testCase "huffman-tree-merge-1" $ do
        let t1 = leaf 2 'Z'
            t2 = leaf 7 'K'
            merged = t1 <> t2
            expected = Node 9 t1 t2
        assertEqual "t1 and t2 should be equal" expected merged,
      testCase "huffman-tree-merge-2" $ do
        let t1 =
              Node
                9
                (leaf 2 'Z')
                (leaf 7 'K')
            t2 = leaf 24 'M'
            merged = t1 <> t2
            expected = Node 33 t1 t2
        assertEqual "t1 and t2 should be equal" expected merged,
      testCase "huffman-tree-merge-3" $ do
        let t1 = leaf 32 'C'
            t2 =
              Node
                33
                ( Node
                    9
                    (leaf 2 'Z')
                    (leaf 7 'K')
                )
                (leaf 24 'M')
            merged = t1 <> t2
            expected = Node 65 t1 t2
        assertEqual "t1 and t2 should be equal" expected merged,
      testCase "huffman-tree-merge-4" $ do
        let t1 = Node 79 (leaf 37 'U') (leaf 42 'D')
            t2 =
              Node
                107
                (leaf 42 'L')
                ( Node
                    65
                    (leaf 32 'C')
                    ( Node
                        33
                        (Node 9 (leaf 2 'Z') (leaf 7 'K'))
                        (leaf 24 'M')
                    )
                )
            merged = t1 <> t2
            expected = Node 186 t1 t2
        assertEqual "t1 and t2 should be equal" expected merged
    ]

treeFunctorTest :: TestTree
treeFunctorTest =
  testCase "huffman-tree-functor-test" $ do
    let aTree =
          Node
            2
            (leaf 3 'A')
            ( Node
                4
                ( Node
                    7
                    (leaf 8 'I')
                    (leaf 1 'Y')
                )
                (leaf 10 'P')
            )
        expected =
          Node
            2
            (Leaf (3, 'A'))
            ( Node
                4
                ( Node
                    7
                    (Leaf (8, 'I'))
                    (Leaf (1, 'Y'))
                )
                (Leaf (10, 'P'))
            )
        tree' = w2c <$> aTree
    assertEqual "tree of word8 to tree of char" expected tree'

tree :: Tree Word8
tree =
  Node
    307
    (leaf 120 'E')
    ( Node
        187
        ( Node
            79
            (leaf 37 'U')
            (leaf 42 'D')
        )
        ( Node
            108
            (leaf 43 'L')
            ( Node
                65
                (leaf 32 'C')
                ( Node
                    33
                    ( Node
                        9
                        (leaf 2 'Z')
                        (leaf 7 'K')
                    )
                    (leaf 24 'M')
                )
            )
        )
    )

treeCreationTest :: TestTree
treeCreationTest = testCase "huffman-tree-creation-from-list" $ do
  let l =
        [ leaf 32 'C',
          leaf 42 'D',
          leaf 120 'E',
          leaf 7 'K',
          leaf 43 'L',
          leaf 24 'M',
          leaf 37 'U',
          leaf 2 'Z'
        ]
      actual = buildTree $ PQ.fromList l
      expected = tree
  case actual of
    Nothing -> assertFailure "A tree is expected"
    Just t -> assertEqual "trees should be equal" expected t

bitBufferTest :: TestTree
bitBufferTest =
  testGroup
    "huffman-bit-buffer"
    [ testCase "huffman-bit-buffer-1-byte" $ do
        let toPush =
              [ BitOn, --- 1 this should be the first one
                BitOff, -- 0
                BitOff, -- 0
                BitOn, --- 1
                BitOn, --- 1
                BitOn, --- 1
                BitOn, --- 1
                BitOff --- 0
              ]
            bf = foldl' bfPush bfEmpty toPush
            b0 = 1 .|. (1 `shiftL` 3) .|. (1 `shiftL` 4) .|. (1 `shiftL` 5) .|. (1 `shiftL` 6)
            expected = BS.reverse $ b0 `BS.cons` BS.empty
            bbs = bfAllBytes bf
        assertEqual "bytes strings should be equal" (bsToBinaryString expected) (bsToBinaryString bbs),
      testCase "huffman-bit-buffer-1-bit" $ do
        let bf = bfPush bfEmpty BitOn
            expected = "00000001"
            bbs = bfAllBytes bf
        assertEqual "bytes strings should be equal" expected (bsToBinaryString bbs),
      testCase "huffman-bit-buffer-1-and-a-half-bytes" $ do
        let toPush =
              [ BitOn, --- 1 this should be the first one
                BitOff, -- 0
                BitOff, -- 0
                BitOn, --- 1
                BitOn, --- 1
                BitOn, --- 1
                BitOn, --- 1
                BitOff, -- 0 end of first byte
                BitOff, -- 0
                BitOff, -- 0
                BitOn, --- 1
                BitOn ---- 1
              ]
            bf = foldl' bfPush bfEmpty toPush
            b0 = 1 .|. (1 `shiftL` 3) .|. (1 `shiftL` 4) .|. (1 `shiftL` 5) .|. (1 `shiftL` 6)
            b1 = (1 `shiftL` 2) .|. (1 `shiftL` 3)
            expected = b0 `BS.cons` b1 `BS.cons` BS.empty
            bbs = bfAllBytes bf
        assertEqual "bytes strings should be equal" expected bbs
    ]

codesTest :: TestTree
codesTest =
  testGroup
    "huffman-codes"
    [ testCase "huffman-codes-E" $ do
        let cs = codes tree
        case M.lookup (c2w 'E') cs of
          Nothing -> assertFailure "Expected values"
          Just k -> assertEqual "should be equal" [BitOff] k,
      testCase "huffman-codes-U" $ do
        let cs = codes tree
        case M.lookup (c2w 'U') cs of
          Nothing -> assertFailure "Expected values"
          Just k -> assertEqual "should be equal" [BitOn, BitOff, BitOff] k,
      testCase "huffman-codes-D" $ do
        let cs = codes tree
        case M.lookup (c2w 'D') cs of
          Nothing -> assertFailure "Expected values"
          Just k -> assertEqual "should be equal" [BitOn, BitOff, BitOn] k,
      testCase "huffman-codes-L" $ do
        let cs = codes tree
        case M.lookup (c2w 'L') cs of
          Nothing -> assertFailure "Expected values"
          Just k -> assertEqual "should be equal" [BitOn, BitOn, BitOff] k,
      testCase "huffman-codes-C" $ do
        let cs = codes tree
        case M.lookup (c2w 'C') cs of
          Nothing -> assertFailure "Expected values"
          Just k -> assertEqual "should be equal" [BitOn, BitOn, BitOn, BitOff] k,
      testCase "huffman-codes-Z" $ do
        let cs = codes tree
        case M.lookup (c2w 'Z') cs of
          Nothing -> assertFailure "Expected values"
          Just k -> assertEqual "should be equal" [BitOn, BitOn, BitOn, BitOn, BitOff, BitOff] k,
      testCase "huffman-codes-M" $ do
        let cs = codes tree
        case M.lookup (c2w 'M') cs of
          Nothing -> assertFailure "Expected values"
          Just k -> assertEqual "should be equal" [BitOn, BitOn, BitOn, BitOn, BitOn] k,
      testCase "huffman-codes-T" $ do
        let cs = codes tree
        case M.lookup (c2w 'T') cs of
          Nothing -> pure ()
          Just k -> assertFailure $ "expected nothing, got " <> show k
    ]

huffmanTests :: TestTree
huffmanTests =
  testGroup
    "huffman"
    [ countFreqTest,
      treeMergeTest,
      treeCreationTest,
      treeFunctorTest,
      bitBufferTest,
      codesTest
    ]
