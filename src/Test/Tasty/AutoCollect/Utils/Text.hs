{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.Utils.Text (
  withoutPrefix,
  withoutSuffix,
  listify,
  quoted,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

withoutPrefix :: Text -> Text -> Text
withoutPrefix pre s = fromMaybe s $ Text.stripPrefix pre s

withoutSuffix :: Text -> Text -> Text
withoutSuffix post s = fromMaybe s $ Text.stripSuffix post s

-- | Convert a list @["a", "b"]@ to the text @"[\"a\", \"b\"]"@.
listify :: [Text] -> Text
listify xs = "[" <> Text.intercalate ", " xs <> "]"

quoted :: Text -> Text
quoted s = "\"" <> s <> "\""
