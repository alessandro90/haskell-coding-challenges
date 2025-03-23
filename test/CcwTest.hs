module CcwTest (parseArgsTests) where

import Ccw
import Data.Either (isLeft)
import Test.Tasty
import Test.Tasty.HUnit

parseArgsEmptyString :: TestTree
parseArgsEmptyString = testCase "ccw-parse-args-empty-string" $ do
  isLeft (parseOptions [[]]) @? "Should return left"

parseNoArgs :: TestTree
parseNoArgs =
  testCase "ccw-parse-args-no-args" $
    assertEqual "Should be equal" (parseOptions []) (Right (defaultOptions, []))

parseInvalidArgs :: TestTree
parseInvalidArgs =
  testCase "ccw-parse-args-invalid" $
    assertBool "Should be Left" $
      isLeft $
        parseOptions ["-x"]

parseValidArgsSeparate :: TestTree
parseValidArgsSeparate =
  testCase "ccw-parse-args-separated" $ do
    let res = parseOptions ["-c", "-l", "-w", "-m", "file1", "file2"]
    case res of
      Left _ -> assertFailure "Right is expected"
      Right (opts, fnames) -> do
        assertEqual "Should count bytes" (countBytes opts) $ Just ()
        assertEqual "Should count lines" (countLines opts) $ Just ()
        assertEqual "Should count words" (countWords opts) $ Just ()
        assertEqual "Should count chars" (countChars opts) $ Just ()
        assertEqual "Should have 2 file names" fnames ["file1", "file2"]

parseValidArgsGrouped :: TestTree
parseValidArgsGrouped =
  testCase "ccw-parse-args-grouped" $ do
    let res = parseOptions ["-lwmc"]
    case res of
      Left _ -> assertFailure "Right is expected"
      Right (opts, fnames) -> do
        assertEqual "Should count bytes" (countBytes opts) $ Just ()
        assertEqual "Should count lines" (countLines opts) $ Just ()
        assertEqual "Should count words" (countWords opts) $ Just ()
        assertEqual "Should count chars" (countChars opts) $ Just ()
        assertBool "Should have 0 file names" $ null fnames

parseArgsTests :: TestTree
parseArgsTests =
  testGroup
    "parse-args"
    [ parseArgsEmptyString,
      parseNoArgs,
      parseInvalidArgs,
      parseValidArgsSeparate,
      parseValidArgsGrouped
    ]
