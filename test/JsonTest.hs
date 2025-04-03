{-# LANGUAGE OverloadedStrings #-}

module JsonTest (jsonTests) where

import qualified Data.Map.Strict as M
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
        let a = runParser array "array" "  [1, 10.5   ,\n\n\t-10 ]  "
        case a of
          Left e -> assertFailure $ errorBundlePretty e
          Right a' ->
            let firstItem = JNumber 1.0
                secondItem = JNumber 10.5
                thirdItem = JNumber $ -10.0
             in assertEqual "should be equal" [firstItem, secondItem, thirdItem] a',
      testCase "json-parse-array-heterogeneous-no-object" $ do
        let a = runParser array "array" "  [1, null, false, \"hello\", [-2]]  "
        case a of
          Left e -> assertFailure $ errorBundlePretty e
          Right a' ->
            let firstItem = JNumber 1.0
                secondItem = JNull
                thirdItem = JBool False
                fourthItem = JText "hello"
                fifthItem = JArray [JNumber $ -2]
             in assertEqual
                  "should be equal"
                  [firstItem, secondItem, thirdItem, fourthItem, fifthItem]
                  a',
      testCase "json-parse-array-of-objects" $ do
        let arr =
              "[\
              \  {\"attr_1\": 10},\
              \  { \
              \      \"attr_2\": true,\
              \      \"attr_3\": \"hello\",\
              \      \"attr_4\": [1, 4]\
              \  } \
              \]"
        let a = runParser array "array" arr
        case a of
          Left e -> assertFailure $ errorBundlePretty e
          Right a' -> do
            assertEqual "len should be 2" 2 $ length a'
            case head a' of
              JObject m -> checkMapKey "attr_1" (JNumber 10) m
              e -> assertFailure $ "Should get an object, got " <> show e
            let secondItem = a' !! 1
            case secondItem of
              JObject m -> do
                checkMapKey "attr_2" (JBool True) m
                checkMapKey "attr_3" (JText "hello") m
                checkMapKey "attr_4" (JArray [JNumber 1, JNumber 4]) m
              e -> assertFailure $ "Should get an object, got " <> show e
    ]

parseObject :: TestTree
parseObject =
  testGroup
    "json-parse-array"
    [ testCase "json-parse-object-empty" $ do
        let o = runParser object "object" "{}"
        case o of
          Left e -> assertFailure $ errorBundlePretty e
          Right o' -> assertBool "should be empty" $ M.null o',
      testCase "json-parse-object-invalid-key" $ do
        let o = runParser object "object" "{1: 2}"
        case o of
          Left _ -> pure ()
          Right o' -> assertFailure $ "should fail " <> show o',
      testCase "json-parse-object-invalid-value" $ do
        let o = runParser object "object" "{\"k\": x}"
        case o of
          Left _ -> pure ()
          Right o' -> assertFailure $ "should fail " <> show o',
      testCase "json-parse-object-single-numeric" $ do
        let o = runParser object "object" "  {  \"attr\"  : 10  }  "
        case o of
          Left e -> assertFailure $ errorBundlePretty e
          Right o' -> do
            let attr = M.lookup "attr" o'
             in assertEqual "should be equal" (Just $ JNumber 10) attr,
      testCase "json-parse-object-multi-values-no-recursion" $ do
        let json =
              "{\
              \  \"attr_1\": 10,\
              \  \"attr_2\": true,\
              \  \"attr_3\": null,\
              \  \"attr_4\": [],\
              \  \"attr_5\": [1, false]\
              \}"
        let o = runParser object "object" json
        case o of
          Left e -> assertFailure $ errorBundlePretty e
          Right o' -> do
            checkMapKey "attr_1" (JNumber 10) o'
            checkMapKey "attr_2" (JBool True) o'
            checkMapKey "attr_3" JNull o'
            checkMapKey "attr_4" (JArray []) o'
            checkMapKey "attr_5" (JArray [JNumber 1, JBool False]) o'
    ]

parseJsonFn :: TestTree
parseJsonFn =
  testGroup
    "json-parse-main-fn"
    [ testCase "json-parse-main-fn-empty" $ do
        let p = parseJson "{}"
        case p of
          Left e -> assertFailure e
          Right json -> assertBool "json should be empty" $ M.null json,
      testCase "json-parse-main-fn-complex" $ do
        let str =
              "{\
              \  \"key_1\": 10,\
              \  \"key_2\": true,\
              \  \"key_3\": null,\
              \  \"key_4\": {\
              \      \"attr_0\": [\
              \          1,\
              \          2,\
              \          [],\
              \          \"hello this is text\"\
              \      ] \
              \  }\
              \}"
        let p = parseJson str
        case p of
          Left e -> assertFailure e
          Right json -> do
            let arrItem0 = JNumber 1
                arrItem1 = JNumber 2
                arrItem2 = JArray []
                arrItem3 = JText "hello this is text"
            let (attr_0, arr) = ("attr_0", JArray [arrItem0, arrItem1, arrItem2, arrItem3])
            let (key_1, value_1) = ("key_1", JNumber 10)
                (key_2, value_2) = ("key_2", JBool True)
                (key_3, value_3) = ("key_3", JNull)
            checkMapKey key_1 value_1 json
            checkMapKey key_2 value_2 json
            checkMapKey key_3 value_3 json
            let innerObj = M.lookup "key_4" json
            case innerObj of
              Nothing -> assertFailure "expected inner object"
              Just obj -> case obj of
                JObject obj' -> do
                  checkMapKey attr_0 arr obj'
                err -> assertFailure $ "expected an object, got " <> show err
    ]

jsonTests :: TestTree
jsonTests =
  testGroup
    "json"
    [ parseNumber,
      parseNull,
      parseBoolean,
      parseText,
      parseArray,
      parseObject,
      parseJsonFn
    ]

checkMapKey :: String -> Value -> M.Map String Value -> IO ()
checkMapKey k v m = case M.lookup k m of
  Nothing -> assertFailure $ "Key " <> k <> " not found"
  Just v' -> assertEqual "should be equal" v v'
