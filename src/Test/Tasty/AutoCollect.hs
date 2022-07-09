{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect (
  processFile,
) where

import Data.Text (Text)
import qualified Data.Text as Text

import Test.Tasty.AutoCollect.GenerateMain
import Test.Tasty.AutoCollect.ModuleType
import Test.Tasty.AutoCollect.Utils.Text

-- | Preprocess the given Haskell file. See Preprocessor.hs
processFile :: FilePath -> Text -> IO Text
processFile path file =
  case parseModuleType file of
    Just (ModuleMain cfg) -> generateMainModule cfg path
    Just ModuleTest ->
      pure . Text.unlines $
        [ "{-# OPTIONS_GHC -fplugin=Test.Tasty.AutoCollect.ConvertTest #-}"
        , file'
        ]
    Nothing -> pure file'
  where
    file' =
      Text.unlines
        [ "{-# LINE 1 " <> quoted (Text.pack path) <> " #-}"
        , file
        ]
