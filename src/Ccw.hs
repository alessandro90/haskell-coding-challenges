module Ccw where

import Data.Bifunctor (Bifunctor (second))
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Misc (startsWithItem)
import System.Environment (getArgs)
import System.IO.Error (catchIOError)

data Options = Options
  { countBytes :: Maybe Bool,
    countLines :: Maybe Bool,
    countWords :: Maybe Bool,
    countChars :: Maybe Bool,
    fileNames :: [String]
  }

defaultOptions :: Options
defaultOptions =
  Options
    { countBytes = Nothing,
      countLines = Nothing,
      countWords = Nothing,
      countChars = Nothing,
      fileNames = []
    }

parseOptions :: [String] -> Either String Options
parseOptions = go defaultOptions
  where
    go args [] = Right args
    go args (s : xs)
      | null s = Left "Empty string"
      | startsWithItem '-' s = parseCommand args (drop 1 s) >>= flip go xs
      | otherwise = go args {fileNames = s : fileNames args} xs

    parseCommand args (s : xs) = case s of
      'c' -> parseCommand args {countBytes = Just True} xs
      'l' -> parseCommand args {countLines = Just True} xs
      'w' -> parseCommand args {countWords = Just True} xs
      'm' -> parseCommand args {countChars = Just True} xs
      e -> Left $ "ERROR: invalid command '" <> show e <> "'"
    parseCommand args [] = Right args

data TextData = TextData
  { textDataBytes :: Maybe Int,
    textDataLines :: Maybe Int,
    textDataWords :: Maybe Int,
    textDataChars :: Maybe Int
  }

getTextData :: Options -> Text -> TextData
getTextData (Options bytes lns wrds chars _) text =
  let bytesNr = bytes >> pure (B.length $ T.encodeUtf8 text)
      linesNr = lns >> pure (length $ T.lines text)
      wordsNr = wrds >> pure (length $ T.words text)
      charsNr = chars >> pure (T.length text)
   in TextData bytesNr linesNr wordsNr charsNr

fetchText :: [FilePath] -> IO [(FilePath, Text)]
fetchText [] = fmap (\x -> [("", x)]) T.pack <$> getContents
fetchText fnames = mapM go fnames
  where
    go fname = (fname,) . T.decodeUtf8 <$> B.readFile fname

runCcw :: IO (Either String [(FilePath, TextData)])
runCcw = do
  options <- fmap parseOptions getArgs
  let fnames = fmap fileNames options
  textContents <- mapM fetchText fnames `catchIOError` (pure . Left . show)
  pure $ extractData options textContents
  where
    extractData options textContents = do
      opts <- options
      map (second $ getTextData opts) <$> textContents
