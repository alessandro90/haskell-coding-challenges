{-# LANGUAGE OverloadedStrings #-}

module Json where

import Control.Monad (void)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data JsonValue
  = JBool Bool
  | JNumber Double
  | JText Text
  | JArray JsonArray
  | JObject JsonObject
  | JNull

type JsonArray = [JsonValue]

type JsonObject = M.Map String JsonValue

null_ :: Parser ()
null_ = space >> void (string "null")

number :: Parser Double
number =
  space
    >> L.signed
      space
      ( try L.float <|> fromIntegral @Int <$> L.decimal
      )

boolean :: Parser Bool
boolean = space >> (True <$ string "true") <|> (False <$ string "false")

text :: Parser Text
text = space >> char '"' *> (T.pack <$> manyTill L.charLiteral (char '"'))

array :: Parser JsonArray
array = space >> between (wschar '[') (wschar ']') (commaSep value)

object :: Parser JsonObject
object = do
  _ <- space
  kv <- between (wschar '{') (wschar '}') (commaSep keyValue)
  pure $ foldr (\(k, v) m -> M.insert k v m) M.empty kv

parseJson :: Text -> Either String JsonObject
parseJson = first errorBundlePretty . runParser (object <* eof) "json"

wschar :: Char -> Parser Char
wschar c = space >> char c

surrounded :: Parser Char -> Parser a -> Parser a
surrounded by p = by *> p <* by

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` wschar ','

key :: Parser String
key = T.unpack <$> (space >> text)

value :: Parser JsonValue
value =
  (JBool <$> boolean)
    <|> (JNull <$ null_)
    <|> (JNumber <$> number)
    <|> (JText <$> text)
    <|> (JArray <$> array)
    <|> (JObject <$> object)

keyValue :: Parser (String, JsonValue)
keyValue = do
  k <- key
  _ <- wschar ':'
  v <- value
  pure (k, v)
