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
    Just (ModuleMain cfg) -> addLinePragma <$> generateMainModule cfg path file
    Just ModuleTest ->
      pure
        . addLine "{-# OPTIONS_GHC -fplugin=Test.Tasty.AutoCollect.ConvertTest #-}"
        . addLinePragma
        $ file
    Nothing -> pure $ addLinePragma file
  where
    addLine line f = line <> "\n" <> f
    -- this is needed to tell GHC to use original path in error messages
    addLinePragma = addLine $ "{-# LINE 1 " <> quoted (Text.pack path) <> " #-}"
