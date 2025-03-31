{-# LANGUAGE OverloadedStrings #-}

module JsonTest (jsonTests) where

import Json
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Prelude hiding (null)

parseNull :: TestTree
parseNull =
  testGroup
    "json-parse-null"
    [ testCase "json-parse-null-no-space" $ do
        let n = runParser null "null" "null"
        case n of
          Right _ -> pure ()
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-null-with-space" $ do
        let n = runParser null "null" "     null"
        case n of
          Right _ -> pure ()
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-null-empty" $ do
        let n = runParser null "null" ""
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
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-boolean-invalid-uppercase" $ do
        let n = runParser boolean "boolean" "False"
        case n of
          Right b -> assertFailure $ "Should fail" <> show b
          Left _ -> pure (),
      testCase "json-parse-boolean-invalid-typo" $ do
        let n = runParser boolean "boolean" "Failse"
        case n of
          Right b -> assertFailure $ "should fail" <> show b
          Left _ -> pure ()
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
          Left e -> assertFailure $ errorBundlePretty e,
      testCase "json-parse-text-missing-closing-quote" $ do
        let n = runParser text "text" "     \"some text \nbetween quotes"
        case n of
          Right t -> assertFailure $ "Missing quote should cause failure" <> show t
          Left _ -> pure ()
    ]

parseArray :: TestTree
parseArray =
  testGroup
    "json-parse-array"
    [ testCase "json-parse-array-empty" $ do
        let a = runParser array "array" "[]"
        case a of
          Left e -> assertFailure $ errorBundlePretty e
          Right a' -> assertEqual "should be empty" [] a',
      testCase "json-parse-array-invalid-missing-closing" $ do
        let a = runParser array "array" "[1, "
        case a of
          Left _ -> pure ()
          Right a' -> assertFailure $ "Missing closing ']' should fail" <> show a',
      testCase "json-parse-array-invalid-missing-comma" $ do
        let a = runParser array "array" "[1 2]"
        case a of
          Left _ -> pure ()
          Right a' -> assertFailure $ "Missing comma should fail" <> show a',
      testCase "json-parse-array-invalid-wrong-data" $ do
        let a = runParser array "array" "[1, x]"
        case a of
          Left _ -> pure ()
          Right a' -> assertFailure $ "Invalid data should fail" <> show a',
      testCase "json-parse-array-of-num" $ do
        let a = runParser array "array" "  [1, 10.5,\n\n\t-10 ]  "
        case a of
          Left e -> assertFailure $ errorBundlePretty e
          Right a' -> do
            let firstItem = JNumber 1.0
                secondItem = JNumber 10.5
                thirdItem = JNumber $ -10.0
            assertEqual "should be equal" [firstItem, secondItem, thirdItem] a',
      testCase "json-parse-array-heterogeneous-no-object" $ do
        let a = runParser array "array" "  [1, null, false, \"hello\", [-2]]  "
        case a of
          Left e -> assertFailure $ errorBundlePretty e
          Right a' -> do
            let firstItem = JNumber 1.0
                secondItem = JNull
                thirdItem = JBool False
                fourthItem = JText "hello"
                fifthItem = JArray [JNumber $ -2]
            assertEqual
              "should be equal"
              [firstItem, secondItem, thirdItem, fourthItem, fifthItem]
              a'
              -- todo: add array of objects tests
    ]

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
