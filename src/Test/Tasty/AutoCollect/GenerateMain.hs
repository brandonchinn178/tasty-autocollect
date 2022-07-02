{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.GenerateMain (
  generateMainModule,
) where

import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, splitExtensions, takeDirectory, (</>))

import Test.Tasty.AutoCollect.Config
import Test.Tasty.AutoCollect.Constants
import Test.Tasty.AutoCollect.Utils.Text
import qualified Test.Tasty.AutoCollect.Utils.TreeMap as TreeMap

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
  case cfgGroupType of
    AutoCollectGroupFlat ->
      -- concat
      --   [ My.Module.Test1.tests
      --   , My.Module.Test2.tests
      --   , ...
      --   ]
      "concat " <> listify (map snd testModulesInfo)
    AutoCollectGroupModules ->
      -- [ testGroup "My.Module.Test1" My.Module.Test1.tests
      -- , testGroup "My.Module.Test2" My.Module.Test2.tests
      -- ]
      listify . flip map testModulesInfo $ \(testModuleDisplay, testsIdentifier) ->
        Text.unwords ["testGroup", quoted testModuleDisplay, testsIdentifier]
    AutoCollectGroupTree ->
      -- [ testGroup "My"
      --     [ testGroup "Module"
      --         [ testGroup "Test1" My.Module.Test1.tests
      --         , testGroup "Test2" My.Module.Test2.tests
      --         ]
      --     ]
      -- ]
      TreeMap.foldTreeMap testGroupFromTree . TreeMap.fromList $ map (first (Text.splitOn ".")) testModulesInfo
  where
    -- List of pairs representing (display name of module, 'tests' identifier)
    testModulesInfo =
      flip map testModules $ \testModule ->
        ( withoutSuffix cfgStripSuffix testModule
        , testModule <> "." <> Text.pack testListIdentifier
        )

    testGroupFromTree mTestsIdentifier subTrees =
      let subGroups =
            flip map (Map.toAscList subTrees) $ \(testModuleDisplay, subTests) ->
              Text.unwords ["testGroup", quoted testModuleDisplay, "$", subTests]
       in case (subGroups, mTestsIdentifier) of
            (subGroups', Nothing) -> listify subGroups'
            ([], Just testsIdentifier) -> testsIdentifier
            (subGroups', Just testsIdentifier) -> "concat " <> listify [listify subGroups', testsIdentifier]

{----- Helpers -----}

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fp = fmap concat . mapM (go . (fp </>)) =<< listDirectory fp
  where
    go child = do
      isDir <- doesDirectoryExist child
      if isDir
        then listDirectoryRecursive child
        else pure [child]
