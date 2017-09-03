module Explore where

import           Text.Pandoc

{-|
  convert markdown to html
  convert returns a either of pandoc error or html string
  convert gives pandoc error in case markdown parsing fails
-}

convert :: String -> Either PandocError String
convert markdownStr =
  fmap
    (writeHtmlString (def {writerHtml5 = True, writerHighlight = True}))
    (readMarkdown (def {readerStandalone = True}) markdownStr)

convertHandlingError :: String -> String
convertHandlingError str = handleError $ convert str


