module Cut where

-- This would probably be much easier using a parser lib like Megaparsec
-- but I tried as an exercise

import Misc (splitStr, splitStrWhen)
import Text.Read (readMaybe)

data Args = Args
  { sep :: Char,
    fields :: [Int],
    files :: [FilePath]
  }
  deriving (Show, Eq)

argsDefault :: Args
argsDefault = Args {sep = '\t', fields = [], files = []}

argsCheckFields :: Args -> Either String Args
argsCheckFields args = case fields args of
  [] -> Left "No fields provided"
  _ -> Right args

parseArgs :: String -> Either String Args
parseArgs s = parseArgs' s argsDefault >>= argsCheckFields

parseArgs' :: String -> Args -> Either String Args
parseArgs' [] args = Right args
parseArgs' s args = go $ skipSpace s
  where
    go [] = Right args
    go str@(x : xs) = case x of
      '-' -> case xs of
        [] -> Right $ args {files = []}
        _ -> parseArg xs args
      ' ' -> parseArgs' xs args
      _ -> Right $ args {files = words str}

parseArg :: String -> Args -> Either String Args
parseArg [] _ = Left "Empty arg"
parseArg (x : xs) args = case x of
  'f' -> goOn parseF -- (skipSpace xs) args
  'd' -> goOn parseD -- (skipSpace xs) args
  _ -> Left $ "Invalid flag " <> show x
  where
    goOn parser = parser (skipSpace xs) args

parseD :: String -> Args -> Either String Args
parseD [] _ = Left "Missing separator"
parseD (delim : xs) args = parseArgs' xs $ args {sep = delim}

parseF :: String -> Args -> Either String Args
parseF [] _ = Left "Missing fields list"
parseF ls@(x : xs) args = case x of
  '"' -> parseFQuoted (skipSpace xs) args
  _ -> parseFUnquoted ls args

parseFQuoted :: String -> Args -> Either String Args
parseFQuoted s args =
  let (list, rest, gotQuote) = extractListUntil '"' s
   in if not gotQuote
        then
          Left $ "Missing closing quote: " <> s
        else case parsedNumbers list of
          Nothing -> Left $ "Invalid field list " <> show list
          Just numbers -> parseArgs' rest $ args {fields = numbers}
  where
    parsedNumbers list = mapM readMaybe $ splitStrWhen (\c -> c == ' ' || c == ',') list

parseFUnquoted :: String -> Args -> Either String Args
parseFUnquoted s args =
  let (list, rest, _) = extractListUntil ' ' s
      parsedNumbers = mapM readMaybe $ splitStr ',' list
   in case parsedNumbers of
        Nothing -> Left $ "Invalid field list " <> show list
        Just numbers -> parseArgs' rest $ args {fields = numbers}

skipSpace :: String -> String
skipSpace = dropWhile (== ' ')

-- | Return a tuple of 3 items where
-- item 1. accumulated string
-- item 2. rest, unparsed string
-- item 3. whether the the target char has been found at
-- the end or end of string has been reached. True if char
-- has been found
extractListUntil :: Char -> String -> (String, String, Bool)
extractListUntil c s =
  let (kept, rest, eos) = go s []
   in (reverse kept, rest, eos)
  where
    go (x : c' : xs') acc =
      if c == c'
        then
          (x : acc, xs', True)
        else go xs' $ c' : x : acc
    go [x] acc =
      if c == x
        then
          (acc, [], True)
        else (x : acc, [], False)
    go [] acc = (acc, [], False)
