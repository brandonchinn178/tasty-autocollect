{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.GenerateMain (
  generateMainModule,
) where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, splitExtensions, takeDirectory, (</>))

import Test.Tasty.AutoCollect.Config
import Test.Tasty.AutoCollect.Constants
import Test.Tasty.AutoCollect.Utils.Tree

generateMainModule :: AutoCollectConfig -> FilePath -> IO Text
generateMainModule cfg@AutoCollectConfig{..} path = do
  testModules <- findTestModules path
  pure . Text.unlines $
    [ "{-# OPTIONS_GHC -w #-}"
    , ""
    , "module Main (main) where"
    , ""
    , "import Test.Tasty"
    , Text.unlines $ map ("import qualified " <>) testModules
    , ""
    , "main :: IO ()"
    , "main = defaultMainWithIngredients ingredients (testGroup suiteName tests)"
    , "  where"
    , "    ingredients = " <> ingredients
    , "    suiteName = " <> suiteName
    , "    tests = " <> generateTests cfg testModules
    ]
  where
    ingredients =
      Text.unwords
        [ listify cfgIngredients
        , "++"
        , if cfgIngredientsOverride then "[]" else "defaultIngredients"
        ]

    suiteName = quoted $ fromMaybe (Text.pack path) cfgSuiteName

{- |
Find all test modules using the given path to the Main module.

>>> findTestModules "test/Main.hs"
["My.Module.Test1", "My.Module.Test2", ...]
-}
findTestModules :: FilePath -> IO [Text]
findTestModules path = mapMaybe toModule . filter (/= path) <$> listDirectoryRecursive testDir
  where
    testDir = takeDirectory path
    toModule fp =
      case splitExtensions (makeRelative testDir fp) of
        (name, ".hs") -> Just (Text.replace "/" "." . Text.pack $ name)
        _ -> Nothing

generateTests :: AutoCollectConfig -> [Text] -> Text
generateTests AutoCollectConfig{..} testModules =
  -- TODO: handle cfgStripSuffix
  case cfgGroupType of
    AutoCollectGroupFlat ->
      -- concat
      --   [ My.Module.Test1.tests
      --   , My.Module.Test2.tests
      --   , ...
      --   ]
      "concat " <> listify (map mkTestsIdentifier testModules)
    AutoCollectGroupModules ->
      -- [ testGroup "My.Module.Test1" My.Module.Test1.tests
      -- , testGroup "My.Module.Test2" My.Module.Test2.tests
      -- ]
      listify . flip map testModules $ \testModule ->
        Text.unwords ["testGroup", quoted testModule, mkTestsIdentifier testModule]
    AutoCollectGroupTree ->
      -- [ testGroup "My"
      --     [ testGroup "Module"
      --         [ testGroup "Test1" My.Module.Test1.tests
      --         , testGroup "Test2" My.Module.Test2.tests
      --         ]
      --     ]
      -- ]
      testGroupsFromTree $ toTree (map (Text.splitOn ".") testModules)
  where
    mkTestsIdentifier testModule = testModule <> "." <> Text.pack testListIdentifier

    testGroupsFromTree Tree{fullPath = testModule, ..} =
      ("concat " <>) . listify $
        [ listify
            [ Text.unwords ["testGroup", quoted $ last $ fullPath tree, "$", testGroupsFromTree tree]
            | tree <- subTrees
            ]
        , if exists
            then mkTestsIdentifier $ Text.intercalate "." testModule
            else "[]"
        ]

{----- Helpers -----}

-- | Convert a list @["a", "b"]@ to the text @"[\"a\", \"b\"]"@.
listify :: [Text] -> Text
listify xs = "[" <> Text.intercalate ", " xs <> "]"

quoted :: Text -> Text
quoted s = "\"" <> s <> "\""

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fp = fmap concat . mapM (go . (fp </>)) =<< listDirectory fp
  where
    go child = do
      isDir <- doesDirectoryExist child
      if isDir
        then listDirectoryRecursive child
        else pure [child]
