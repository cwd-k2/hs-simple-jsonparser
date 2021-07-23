module Data.SimpleJson
  ( Json (..)
  , isToken
  ) where

data Json
  = JsonObject [(String, Json)]
  | JsonArray [Json]
  | JsonString String
  | JsonNumber String
  | JsonBool Bool
  | JsonNull
  deriving Show

isToken :: Char -> Bool
isToken '{' = True
isToken '}' = True
isToken '[' = True
isToken ']' = True
isToken ',' = True
isToken ':' = True
isToken _   = False

