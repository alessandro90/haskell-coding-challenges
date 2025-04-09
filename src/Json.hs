{-# LANGUAGE OverloadedStrings #-}

module Json where

import Control.Monad (void)
import Data.Bifunctor (Bifunctor (first))
import Data.Bits (shiftL, (.|.))
import Data.Char (digitToInt)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (null)

type Parser = Parsec Void Text

data Value
  = JBool Bool
  | JNumber Double
  | JText Text
  | JArray [Value]
  | JObject Object
  | JNull
  deriving (Show, Eq)

type Object = M.Map String Value

null :: Parser ()
null = space >> void (string "null")

number :: Parser Double
number = space >> L.signed space unsigned
  where
    unsigned = try L.float <|> fromIntegral @Int <$> L.decimal

delimited :: Char -> Char -> Parser a -> Parser a
delimited l r = between (wschar l) (wschar r)

boolean :: Parser Bool
boolean = space >> (True <$ string "true") <|> (False <$ string "false")

escape :: Parser Char
escape = char '\\' >> escaped
  where
    escaped =
      char '"'
        <|> char '\\'
        <|> char '\"'
        <|> char '/'
        <|> unicode
        <|> (char 'b' >> pure '\b')
        <|> (char 'f' >> pure '\f')
        <|> (char 'n' >> pure '\n')
        <|> (char 'r' >> pure '\r')
        <|> (char 't' >> pure '\t')

    unicode = do
      void $ char 'u'
      d0 <- hexToShiftedInt 12
      d1 <- hexToShiftedInt 8
      d2 <- hexToShiftedInt 4
      d3 <- digitToInt <$> hexDigitChar
      let n = d0 .|. d1 .|. d2 .|. d3
      pure $ toEnum n
      where
        hexToShiftedInt offset =
          flip shiftL offset . digitToInt <$> hexDigitChar

text :: Parser Text
text =
  space
    >> char '"'
      *> ( T.pack
             <$> manyTill (try escape <|> L.charLiteral) (char '"')
         )

array :: Parser [Value]
array = space >> delimited '[' ']' (commaSep $ value <* space)

object :: Parser Object
object = M.fromList <$> (space >> delimited '{' '}' (commaSep $ keyValue <* space))

-- | The main function
parseJson :: Text -> Either String Object
parseJson = first errorBundlePretty . runParser (object <* space <* eof) "json"

wschar :: Char -> Parser Char
wschar c = space >> char c

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` wschar ','

key :: Parser String
key = T.unpack <$> text

value :: Parser Value
value =
  (JBool <$> try boolean)
    <|> (JNull <$ try null)
    <|> (JNumber <$> try number)
    <|> (JText <$> try text)
    <|> (JArray <$> try array)
    <|> (JObject <$> object)

keyValue :: Parser (String, Value)
keyValue = do
  k <- key
  _ <- wschar ':'
  v <- value
  pure (k, v)
