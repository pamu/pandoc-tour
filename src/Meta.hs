module Meta where

import           Text.Pandoc

extractMeta :: String -> Meta
extractMeta markdown = case (handleError . readMarkdown (def {readerStandalone = True})) markdown of
  Pandoc meta _ -> meta

extractTitle :: String -> Maybe String
extractTitle markdown =  fmap (\_ -> "") (lookupMeta "title" (extractMeta markdown))

extractDate :: String -> Maybe String
extractDate = undefined

extractSummary :: String -> Maybe String
extractSummary = undefined

extractPoster :: String -> Maybe String
extractPoster = undefined

