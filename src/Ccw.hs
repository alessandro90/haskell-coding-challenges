module Ccw where

data CmdLnArgs = CmdLnArgs
  { countBytes :: Maybe Bool,
    countLines :: Maybe Bool,
    countWords :: Maybe Bool,
    countChars :: Maybe Bool,
    fileName :: Maybe String
  }

defCmdLnArgs :: CmdLnArgs
defCmdLnArgs =
  CmdLnArgs
    { countBytes = Nothing,
      countLines = Nothing,
      countWords = Nothing,
      countChars = Nothing,
      fileName = Nothing
    }

cmdLnArgs :: [String] -> Either String CmdLnArgs
cmdLnArgs = go defCmdLnArgs
  where
    go args [] = Right args
    go args (s : xs)
      | null s = Left "Empty string"
      | isCommand s = parseCommand args (drop 1 s) >>= flip go xs
      | otherwise = go args {fileName = Just s} xs

    isCommand [] = False
    isCommand (s : _) = s == '-'

    parseCommand args (s : xs) = case s of
      'c' -> parseCommand args {countBytes = Just True} xs
      'l' -> parseCommand args {countLines = Just True} xs
      'w' -> parseCommand args {countWords = Just True} xs
      'm' -> parseCommand args {countChars = Just True} xs
      e -> Left $ "ERROR: invalid command '" <> show e <> "'"
    parseCommand args [] = Right args
