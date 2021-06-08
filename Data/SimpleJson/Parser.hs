module Data.SimpleJson.Parser
  ( JsonParser
  , json
  , parse
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.State (MonadState (get, put), MonadTrans (lift),
                                      StateT (..), evalStateT, gets)
import           Data.Char           (isDigit, isSpace)
import           Data.Map            (fromList)

import           Data.SimpleJson     (Json (..), isToken)

type JsonParser a = StateT String Maybe a

json :: JsonParser Json
json = (jsonObject <* skip <* eos) <|> (jsonArray <* skip <* eos) <|> lift Nothing

parse :: JsonParser Json -> String -> Maybe Json
parse = evalStateT

skip :: JsonParser ()
skip = StateT $ \str -> Just ((), dropWhile isSpace str)

eos :: JsonParser ()
eos = StateT $ \str ->
  if null str
     then Just ((), [])
     else Nothing

char :: Char -> JsonParser ()
char c = StateT $ \str ->
  if not (null str) && head str == c
     then Just ((), tail str)
     else Nothing

char' :: Char -> JsonParser ()
char' c = skip *> char c

readUntil :: (Char -> Bool) -> JsonParser String
readUntil p = do
  (str, rest) <- gets $ break p
  put rest
  return str

jsonElement :: JsonParser Json
jsonElement =
  jsonObject
  <|> jsonArray
  <|> jsonString
  <|> jsonNumber
  <|> jsonBool
  <|> jsonNull
  <|> lift Nothing

jsonString :: JsonParser Json
jsonString = JsonString <$> (char' '"' *> readUntil (== '"') <* char '"')

jsonNumber :: JsonParser Json
jsonNumber = JsonNumber <$> (float <|> integer)
  where
    nonEmpty p = do
      s <- p
      if null s
         then lift Nothing
         else return s
    float = do
      decimal  <- skip     *> nonEmpty (readUntil (not . isDigit))
      fraction <- char '.' *> nonEmpty (readUntil (not . isDigit))
      return $ decimal <> "." <> fraction
    integer = skip *> nonEmpty (readUntil (not . isDigit))

jsonBool :: JsonParser Json
jsonBool = do
  s <- skip *> readUntil ((||) <$> isSpace <*> isToken)
  case s of
    "true"  -> return $ JsonBool True
    "false" -> return $ JsonBool False
    _       -> lift Nothing

jsonNull :: JsonParser Json
jsonNull = do
  s <- skip *> readUntil ((||) <$> isSpace <*> isToken)
  if s == "null"
     then return JsonNull
     else lift Nothing

jsonArray :: JsonParser Json
jsonArray = JsonArray <$> (char' '[' *> elements <* char' ']')
    where
      elements = do
        s <- get
        case runStateT jsonElement s of
          Just (j, s') -> put s' >> (j :) <$> elements'
          Nothing      -> return []
      elements' = do
        s <- get
        case runStateT (char' ',' *> jsonElement) s of
          Just (j, s') -> put s' >> (j :) <$> elements'
          Nothing      -> return []

jsonObject :: JsonParser Json
jsonObject = JsonObject . fromList <$> (char' '{' *> elements <* char' '}')
    where
      string = char' '"' *> readUntil (== '"') <* char '"'
      elements = do
        s <- get
        case runStateT ((,) <$> string <*> (char' ':' *> jsonElement)) s of
          Just (j, s') -> put s' >> (j :) <$> elements'
          Nothing      -> return []
      elements' = do
        s <- get
        case runStateT ((,) <$> (char' ',' *> string) <*> (char ':' *> jsonElement)) s of
          Just (j, s') -> put s' >> (j :) <$> elements'
          Nothing      -> return []