module Ccw where

import Data.Bifunctor (Bifunctor (second))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Functor (($>), (<&>))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Misc (digitsNr, startsWithItem)
import System.Environment (getArgs)
import System.Exit (ExitCode, exitFailure, exitSuccess)
import System.IO (getContents')
import System.IO.Error (catchIOError)
import Text.Printf (printf)

runCcw :: IO ExitCode
runCcw = obtainData >>= printAndExit . programOutput
  where
    programOutput textData = textData <&> formatOutput
    printAndExit (Left e) = putStrLn e >> exitFailure
    printAndExit (Right r) = putStrLn r >> exitSuccess

data Options = Options
  { countBytes :: Maybe (),
    countLines :: Maybe (),
    countWords :: Maybe (),
    countChars :: Maybe ()
  }
  deriving (Eq, Show)

emptyOptions :: Options
emptyOptions =
  Options
    { countBytes = Nothing,
      countLines = Nothing,
      countWords = Nothing,
      countChars = Nothing
    }

defaultOptions :: Options
defaultOptions =
  Options
    { countBytes = Just (),
      countLines = Just (),
      countWords = Just (),
      countChars = Nothing
    }

areOptionsEmpty :: Options -> Bool
areOptionsEmpty (Options b l w c) =
  isNothing b && isNothing l && isNothing w && isNothing c

optionsAlt :: Options -> Options -> Options
optionsAlt opts1 opts2 =
  if areOptionsEmpty opts1 then opts2 else opts1

parseOptions :: [String] -> Either String (Options, [FilePath])
parseOptions = go (emptyOptions, [])
  where
    go (opts, fnames) [] =
      let opts' = opts `optionsAlt` defaultOptions
       in Right (opts', reverse fnames)
    go (opts, fnames) (s : xs)
      | null s = Left "Empty string"
      | startsWithItem '-' s =
          parseCommand opts (drop 1 s) >>= flip go xs . (,fnames)
      | otherwise = go (opts, s : fnames) xs

    parseCommand args (s : xs) = case s of
      'c' -> parseCommand args {countBytes = Just ()} xs
      'l' -> parseCommand args {countLines = Just ()} xs
      'w' -> parseCommand args {countWords = Just ()} xs
      'm' -> parseCommand args {countChars = Just ()} xs
      e -> Left $ "ERROR: invalid command " <> show e
    parseCommand args [] = Right args

data TextData = TextData
  { textDataBytes :: Maybe Int,
    textDataLines :: Maybe Int,
    textDataWords :: Maybe Int,
    textDataChars :: Maybe Int
  }
  deriving (Eq, Show)

getTextData :: Options -> Text -> TextData
getTextData (Options bytes lns wrds chars) text =
  let bytesNr = bytes >> pure (B.length $ TE.encodeUtf8 text)
      linesNr = lns >> pure (BC.count '\n' $ TE.encodeUtf8 text)
      wordsNr = wrds >> pure (length $ T.words text)
      charsNr = chars >> pure (T.length text)
   in TextData bytesNr linesNr wordsNr charsNr

fetchText :: [FilePath] -> IO [(FilePath, Text)]
fetchText [] = fmap (\x -> [("", x)]) T.pack <$> getContents'
fetchText fnames = mapM go fnames
  where
    go fname = (fname,) . TE.decodeUtf8 <$> B.readFile fname

obtainData :: IO (Either String ([(FilePath, TextData)], Options))
obtainData = do
  options <- fmap parseOptions getArgs
  let fnames = fmap snd options
  textContents <- mapM fetchText fnames `catchIOError` (pure . Left . show)
  pure $ extractData (fst <$> options) textContents
  where
    extractData options textContents = do
      opts <- options
      let res = map (second $ getTextData opts) <$> textContents
      (,opts) <$> res

formatOutput :: ([(FilePath, TextData)], Options) -> String
formatOutput (textDataList, options) =
  let total = calcTotal $ map snd textDataList
      formattedLines = map (formatLine total) textDataList
   in unlines $ finalLines total formattedLines
  where
    finalLines _ [] = []
    finalLines _ [l] = [l]
    finalLines total ls@(_ : _) = ls <> [totalLine total options]

    totalLine total@(Total b l w c) (Options tb tl tw tc) =
      let d = TextData (tb $> b) (tl $> l) (tw $> w) (tc $> c)
       in formatLine total ("total", d)

    formatLine (Total totC totL totW totM) (fname, textData) =
      let c = maybe "" (showNum totC) textData.textDataBytes
          l = maybe "" (showNum totL) textData.textDataLines
          w = maybe "" (showNum totW) textData.textDataWords
          m = maybe "" (showNum totM) textData.textDataChars
          fname' = if null fname then "" else '\t' : fname
       in c <> l <> w <> m <> fname'
      where
        showNum tot n = printf "%*d" (digitsNr tot) n <> "\t"

data Total = Total
  { totalBytes :: Int,
    totalLines :: Int,
    totalWords :: Int,
    totalChars :: Int
  }

addTotal :: Total -> Total -> Total
addTotal (Total c l w m) (Total c' l' w' m') =
  Total (c + c') (l + l') (w + w') (m + m')

calcTotal :: [TextData] -> Total
calcTotal = foldr (\x y -> toTotal x `addTotal` y) $ Total 0 0 0 0
  where
    toTotal d =
      Total
        (orZero d.textDataBytes)
        (orZero d.textDataLines)
        (orZero d.textDataWords)
        (orZero d.textDataChars)
    orZero = fromMaybe 0
