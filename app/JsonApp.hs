module Main where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Bifunctor (Bifunctor (second))
import qualified Data.List as L
import qualified Data.Text.IO as T
import GHC.IO.Exception ()
import Json (parseJson)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO.Error (catchIOError)
import Text.Pretty.Simple (pPrint)

main :: IO ExitCode
main = runMain $ do
  files <- getArgs >>= mapM (\fname -> (fname,) <$> T.readFile fname)
  when (L.null files) $ throwIO $ userError "Provide at least a file to parse"
  mapM_ (formatResult . second parseJson) files
  pure ExitSuccess
  where
    runMain action =
      let reportAndFail e = putStrLn ("ERROR: " <> show e) >> pure (ExitFailure 1)
       in action `catchIOError` reportAndFail

    formatResult (fname, parseResult) =
      putStrLn ("Result of parsing file: '" <> fname <> "':\n")
        >> either print pPrint parseResult
