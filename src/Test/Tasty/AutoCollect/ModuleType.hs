{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.ModuleType (
  ModuleType (..),
  parseModuleType,
) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

import Test.Tasty.AutoCollect.Config
import Test.Tasty.AutoCollect.Constants

data ModuleType
  = ModuleMain AutoCollectConfig
  | ModuleTest

parseModuleType :: Text -> Maybe ModuleType
parseModuleType = go . groupWhitespace
  where
    go [] = Nothing
    go ("{-" : _ : x : rest)
      | isMainComment (Text.unpack x) =
          case parseConfig $ Text.concat $ takeWhile (/= "-}") rest of
            Right cfg -> Just (ModuleMain cfg)
            Left e -> errorWithoutStackTrace $ "Could not parse configuration: " <> Text.unpack e
      | isTestComment (Text.unpack x) = Just ModuleTest
    go (_ : rest) = go rest

{- |
Group consecutive whitespace characters.

>>> groupWhitespace " a  bb  c "
[" ", "a", "  ", "bb", "  ", "c", " "]
-}
groupWhitespace :: Text -> [Text]
groupWhitespace = Text.groupBy (\c1 c2 -> isSpace c1 == isSpace c2)
