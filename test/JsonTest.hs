{-# LANGUAGE OverloadedStrings #-}

module JsonTest (jsonTests) where

import Json
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

parseNull :: TestTree
parseNull =
  testGroup
    "json-parse-null"
    [ testCase "json-parse-null-no-space" $ do
        let n = runParser null_ "null" "null"
        case n of
          Right _ -> pure ()
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-null-with-space" $ do
        let n = runParser null_ "null" "     null"
        case n of
          Right _ -> pure ()
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-null-empty" $ do
        let n = runParser null_ "null" ""
        case n of
          Right _ -> assertFailure "Empty string should cause failure"
          Left _ -> pure ()
    ]

parseBoolean :: TestTree
parseBoolean =
  testGroup
    "json-parse-boolean"
    [ testCase "json-parse-boolean-empty" $ do
        let n = runParser boolean "boolean" ""
        case n of
          Right b ->
            assertFailure $
              "Empty string should cause failure, but got " <> show b
          Left _ -> pure (),
      testCase "json-parse-boolean-true" $ do
        let n = runParser boolean "boolean" "true"
        case n of
          Right b -> assertEqual "Should be True" True b
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-boolean-false" $ do
        let n = runParser boolean "boolean" "false"
        case n of
          Right b -> assertEqual "Should be False" False b
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-boolean-false-with-space" $ do
        let n = runParser boolean "boolean" "   \n   false"
        case n of
          Right b -> assertEqual "Should be False" False b
          Left e -> assertFailure $ errorBundlePretty e
    ]

parseNumber :: TestTree
parseNumber =
  testGroup
    "json-parse-number"
    [ testCase "json-parse-number-int" $ do
        let n = runParser number "num" "10"
        case n of
          Right n' -> assertEqual "should be 10" 10 n'
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-number-float" $ do
        let n = runParser number "num" "3.5"
        case n of
          Right n' -> assertEqual "should be 3.5" 3.5 n'
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-number-negative" $ do
        let n = runParser number "num" "-3.5"
        case n of
          Right n' -> assertEqual "should be -3.5" (-3.5) n'
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-number-empty" $ do
        let n = runParser number "num" ""
        case n of
          Right n' -> assertFailure $ "Should have failed, but got " <> show n'
          Left _ -> pure (),
      testCase "json-parse-number-with-space-before" $ do
        let n = runParser number "num" "   10"
        case n of
          Right n' -> assertEqual "should be 10" 10 n'
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-number-with-space-after" $ do
        let n = runParser number "num" "10     "
        case n of
          Right n' -> assertEqual "should be 10" 10 n'
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-number-with-newline" $ do
        let n = runParser number "num" "\n10"
        case n of
          Right n' -> assertEqual "should be 10" 10 n'
          Left e -> assertFailure $ errorBundlePretty e
    ]

parseText :: TestTree
parseText =
  testGroup
    "json-parse-text"
    [ testCase "json-parse-text-empty" $ do
        let n = runParser text "text" ""
        case n of
          Right t ->
            assertFailure $
              "Empty string should cause failure, got " <> show t
          Left _ -> pure (),
      testCase "json-parse-text-no-space" $ do
        let n = runParser text "text" "\"some text \nbetween quotes\""
        case n of
          Right t -> assertEqual "should be equal" "some text \nbetween quotes" t
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-text-with-space" $ do
        let n = runParser text "text" "     \"some text \nbetween quotes\""
        case n of
          Right t -> assertEqual "should be equal" "some text \nbetween quotes" t
          Left e -> assertFailure $ errorBundlePretty e
    ]

parseArray :: TestTree
parseArray = undefined

jsonTests :: TestTree
jsonTests =
  testGroup
    "json"
    [ parseNumber,
      parseNull,
      parseBoolean,
      parseText,
      parseArray
    ]
