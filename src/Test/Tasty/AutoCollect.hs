{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect (
  processFile,
) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

import Test.Tasty.AutoCollect.Config (AutoCollectConfig, parseConfig)
import Test.Tasty.AutoCollect.Constants (isMainComment, isTestComment)
import Test.Tasty.AutoCollect.GenerateMain (generateMainModule)

-- | Preprocess the given Haskell file. See Preprocessor.hs
processFile :: FilePath -> Text -> IO Text
processFile path file =
  case parseModuleType file of
    Just (ModuleMain cfg) -> generateMainModule cfg path
    Just ModuleTest -> pure $ "{-# OPTIONS_GHC -fplugin=Test.Tasty.AutoCollect.Plugin #-}\n" <> file
    Nothing -> pure file

{----- Parse module type -----}

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
