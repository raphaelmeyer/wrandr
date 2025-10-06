module Json (KeyValues, Json (..), parse) where

import Control.Applicative ((<|>))
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Text.ParserCombinators.ReadP as ReadP

type KeyValues = Map.Map Text.Text Json

data Json
  = Array [Json]
  | Object KeyValues
  | String Text.Text
  | Boolean Bool
  | Number Double
  | Null
  deriving (Eq, Show)

parse :: Text.Text -> Maybe Json
parse input = case ReadP.readP_to_S parseAll $ Text.unpack input of
  [(result, _)] -> Just result
  _ -> Nothing

parseAll :: ReadP.ReadP Json
parseAll = json <* ReadP.skipSpaces <* ReadP.eof

json :: ReadP.ReadP Json
json =
  array
    <|> object
    <|> string
    <|> boolean
    <|> number
    <|> Json.null

array :: ReadP.ReadP Json
array = do
  items <-
    ReadP.between (consume '[') (consume ']') $
      ReadP.sepBy json comma
  pure $ Array items

object :: ReadP.ReadP Json
object = do
  items <-
    ReadP.between (consume '{') (consume '}') $
      ReadP.sepBy keyValue comma
  pure $ Object (Map.fromList items)

string :: ReadP.ReadP Json
string = String <$> stringValue

boolean :: ReadP.ReadP Json
boolean = do
  ReadP.skipSpaces
  true <|> false

number :: ReadP.ReadP Json
number = do
  ReadP.skipSpaces
  sign <- ReadP.option id (negate <$ consume '-')
  intPart <- ReadP.munch1 Char.isDigit
  fracPart <- ReadP.option "" $ do
    (:) <$> ReadP.char '.' <*> ReadP.munch1 Char.isDigit
  let value = read (intPart ++ fracPart)
  pure $ Number (sign value)

null :: ReadP.ReadP Json
null = Null <$ ReadP.skipSpaces <* ReadP.string "null"

keyValue :: ReadP.ReadP (Text.Text, Json)
keyValue = do
  key <- stringValue
  consume ':'
  value <- json
  pure (key, value)

stringValue :: ReadP.ReadP Text.Text
stringValue = do
  content <-
    ReadP.between (consume '"') (consume '"') $
      ReadP.many (ReadP.satisfy (/= '"'))
  pure $ Text.pack content

true :: ReadP.ReadP Json
true = Boolean True <$ ReadP.skipSpaces <* ReadP.string "true"

false :: ReadP.ReadP Json
false = Boolean False <$ ReadP.skipSpaces <* ReadP.string "false"

comma :: ReadP.ReadP ()
comma = ReadP.skipSpaces <* ReadP.char ','

consume :: Char -> ReadP.ReadP ()
consume c = ReadP.skipSpaces <* ReadP.char c
