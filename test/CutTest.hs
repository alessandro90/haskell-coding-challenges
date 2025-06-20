module CutTest where

import Cut (Args (fields, files, sep), argsDefault, parseArgs)
import Test.Tasty
import Test.Tasty.HUnit

parseArgsTest :: TestTree
parseArgsTest =
  testGroup
    "cut-parse-args"
    [ testCase "cut-parse-args-no-args" $ do
        let args = parseArgs ""
        case args of
          Left _ -> pure ()
          Right args' -> assertFailure $ "expected error, got : " <> show args',
      testCase "cut-parse-args-field-no-quote" $ do
        let args = parseArgs "-f1"
        case args of
          Left e -> assertFailure $ "got error: " <> e
          Right args' -> assertEqual "should be default" argsDefault {fields = [1]} args',
      testCase "cut-parse-args-fields-no-quote" $ do
        let args = parseArgs "-f1,2,3"
        case args of
          Left e -> assertFailure $ "got error: " <> e
          Right args' -> assertEqual "should be default" argsDefault {fields = [1, 2, 3]} args',
      testCase "cut-parse-args-field-quote" $ do
        let args = parseArgs "-f \"1\""
        case args of
          Left e -> assertFailure $ "got error: " <> e
          Right args' -> assertEqual "should be default" argsDefault {fields = [1]} args',
      testCase "cut-parse-args-fields-space-quote" $ do
        let args = parseArgs "-f \"1 2 3\""
        case args of
          Left e -> assertFailure $ "got error: " <> e
          Right args' -> assertEqual "should be default" argsDefault {fields = [1, 2, 3]} args',
      testCase "cut-parse-args-fields-comma-quote" $ do
        let args = parseArgs "-f \"1, 2, 3\""
        case args of
          Left e -> assertFailure $ "got error: " <> e
          Right args' -> assertEqual "should be default" argsDefault {fields = [1, 2, 3]} args',
      testCase "cut-parse-args-sep-comma" $ do
        let args = parseArgs "-d, -f1"
        case args of
          Left e -> assertFailure $ "got error: " <> e
          Right args' -> assertEqual "should have comma as sep" argsDefault {fields = [1], sep = ','} args',
      testCase "cut-parse-args-single-file" $ do
        let args = parseArgs "-f1 hello.txt"
        case args of
          Left e -> assertFailure $ "got error: " <> e
          Right args' -> assertEqual "should have a file" argsDefault {fields = [1], files = ["hello.txt"]} args',
      testCase "cut-parse-args-multi-file" $ do
        let args = parseArgs "-f1 hello.txt afile.txt"
        case args of
          Left e -> assertFailure $ "got error: " <> e
          Right args' -> assertEqual "should have 2 files" argsDefault {fields = [1], files = ["hello.txt", "afile.txt"]} args'
    ]

cutTests :: TestTree
cutTests =
  testGroup
    "cut"
    [parseArgsTest]
