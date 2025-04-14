{-# LANGUAGE OverloadedStrings #-}

module HuffmanTest where

import Data.ByteString.Internal (w2c)
import qualified Data.PQueue.Min as PQ
import Huffman (Tree (Leaf, Node), buildTree, countFreq, leaf)
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
    let tree =
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
        tree' = w2c <$> tree
    assertEqual "tree of word8 to tree of char" expected tree'

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
      tree = buildTree $ PQ.fromList l
      expected =
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
  case tree of
    Nothing -> assertFailure "A tree is expected"
    Just t -> assertEqual "trees should be equal" expected t

huffmanTests :: TestTree
huffmanTests =
  testGroup
    "huffman"
    [ countFreqTest,
      treeMergeTest,
      treeCreationTest,
      treeFunctorTest
    ]
