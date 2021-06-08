module Data.SimpleJson
  ( Json (..)
  , isToken
  ) where

import           Data.Map (Map)

data Json
  = JsonObject (Map String Json)
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

