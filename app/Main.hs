module Main where

import           Data.Maybe
import           Explore    (convertHandlingError)
import           Meta       (extractSummary, extractMeta, extractTags)

main :: IO ()
--main = interact convertHandlingError
--main = interact (fromMaybe "" . extractSummary)
--main = interact (show . extractMeta)
main = interact (show . extractTags)
