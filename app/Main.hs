module Main where

import           Explore (convertHandlingError)
import           Meta    (extractMeta)

main :: IO ()
--main = interact convertHandlingError
main = interact (show . extractMeta)
