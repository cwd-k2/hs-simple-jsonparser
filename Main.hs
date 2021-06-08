module Main where

import           Data.SimpleJson.Parser (json, parse)

main :: IO ()
main = do
  j <- parse json <$> getContents
  print j
