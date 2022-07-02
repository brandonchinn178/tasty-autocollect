module Test.Tasty.AutoCollect.Utils.Text (
  withoutPrefix,
  withoutSuffix,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

withoutPrefix :: Text -> Text -> Text
withoutPrefix pre s = fromMaybe s $ Text.stripPrefix pre s

withoutSuffix :: Text -> Text -> Text
withoutSuffix post s = fromMaybe s $ Text.stripSuffix post s
