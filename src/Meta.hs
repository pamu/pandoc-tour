module Meta where

import           Control.Monad
import qualified Data.List              as List
import qualified Data.Map               as Map
import           Data.Maybe
import           Prelude
import           Text.Pandoc
import           Text.Pandoc.Definition

extractTitle :: String -> Maybe String
extractTitle = extractKey "title" . extractMeta

extractDate :: String -> Maybe String
extractDate = undefined

extractSummary :: String -> Maybe String
extractSummary = extractKey "summary" . extractMeta

extractPoster :: String -> Maybe String
extractPoster = extractKey "poster" . extractMeta

extractTags :: String -> [String]
extractTags = extractMetaList "tags" . extractMeta

extractMetaList :: String -> Meta -> [String]
extractMetaList key meta =
  case Map.lookup key $ unMeta meta of
    Just (MetaList metaValues) ->
      fmap
        (\metaValue ->
           let x =
                 case metaValue of
                   MetaInlines inlines -> [fromMaybe [] $ convertInlines inlines]
                   _ -> []
           in join x)
        metaValues
    _ -> []


extractMeta :: String -> Meta
extractMeta markdown =
  case (handleError . readMarkdown (def {readerStandalone = True})) markdown of
    Pandoc meta _ -> meta

extractKey :: String -> Meta -> Maybe String
extractKey key meta =
  case Map.lookup key $ unMeta meta of
    Just (MetaInlines inlines) -> convertInlines inlines
    Just (MetaBlocks blocks)   -> convertBlocks blocks
    _                          -> Nothing

extractBlockStr :: MetaValue -> Maybe String
extractBlockStr metaValue =
  case metaValue of
    MetaBlocks blocks -> convertBlocks blocks
    _                 -> Nothing

squash :: [Maybe String] -> Maybe String
squash list =
  let newList = List.filter isJust list
  in if List.null newList
       then Nothing
       else Just $ unwords $ List.map (fromMaybe "") newList

convertBlocks :: [Block] -> Maybe String
convertBlocks = squash . fmap convertBlock

convertBlock :: Block -> Maybe String
convertBlock (Para inlines) = Just $ unwords $ fmap (fromMaybe "" . convertInline) inlines
convertBlock _ = Nothing

convertInline :: Inline -> Maybe String
convertInline (Str str) = Just str
convertInline _         = Nothing

convertInlines :: [Inline] -> Maybe String
convertInlines = squash . fmap convertInline

extractInlineStr :: MetaValue -> Maybe String
extractInlineStr metaValue =
  case metaValue of
    MetaInlines inlines -> convertInlines inlines
    _                   -> Nothing

unfoldLine :: [Inline] -> Maybe String
unfoldLine inlines = extractLineHelper inlines ""
  where
    extractLineHelper :: [Inline] -> String -> Maybe String
    extractLineHelper [] result           = Nothing
    extractLineHelper (Space:xs) result   = Just $ result ++ " "
    extractLineHelper (Str str:xs) result = Just $ result ++ str
