module Main where

import Data.Bifunctor (Bifunctor (second))
import qualified Data.Text.IO as T
import Json (parseJson)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO.Error (catchIOError)
import Text.Pretty.Simple (pPrint)

main :: IO ExitCode
main = runMain $ do
  files <- mapM (\fname -> (fname,) <$> T.readFile fname) <$> getArgs
  results <- fmap (second parseJson) <$> files
  mapM_ formatResult results
  pure ExitSuccess
  where
    runMain :: IO ExitCode -> IO ExitCode
    runMain action =
      let reportAndFail e = putStrLn ("ERROR: " <> show e) >> pure (ExitFailure 1)
       in action `catchIOError` reportAndFail

    formatResult (fname, parseResult) =
      putStrLn ("Result of parsing file: '" <> fname <> "':\n")
        >> either print pPrint parseResult
