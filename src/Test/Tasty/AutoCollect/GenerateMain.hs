{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.GenerateMain (
  generateMainModule,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Test.Tasty.AutoCollect.Config

generateMainModule :: AutoCollectConfig -> FilePath -> IO Text
generateMainModule AutoCollectConfig{..} path = do
  pure . Text.unlines $
    [ "{-# OPTIONS_GHC -w #-}"
    , ""
    , "module Main (main) where"
    , ""
    , "import Test.Tasty"
    , ""
    , "main :: IO ()"
    , "main = defaultMainWithIngredients ingredients (testGroup suiteName [])"
    , "  where"
    , "    ingredients = " <> ingredients
    , "    suiteName = " <> suiteName
    ]
  where
    ingredients =
      Text.unwords
        [ listify cfgIngredients
        , "++"
        , if cfgIngredientsOverride then "[]" else "defaultIngredients"
        ]

    suiteName = quoted $ fromMaybe (Text.pack path) cfgSuiteName

{----- Helpers -----}

-- | Convert a list @["a", "b"]@ to the text @"[\"a\", \"b\"]"@.
listify :: [Text] -> Text
listify xs = "[" <> Text.intercalate ", " xs <> "]"

quoted :: Text -> Text
quoted s = "\"" <> s <> "\""
