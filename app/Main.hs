module Main where

import qualified Explore (convert)

main :: IO ()
main = interact convert

convert :: String -> String
convert str = case Explore.convert str of
  Right convertedStr -> convertedStr
  Left _ -> ""
