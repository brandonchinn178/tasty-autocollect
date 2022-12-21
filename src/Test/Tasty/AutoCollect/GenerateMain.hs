{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.GenerateMain (
  generateMainModule,
) where

import qualified Data.ByteString as ByteString
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, splitExtensions, takeDirectory, (</>))

import Test.Tasty.AutoCollect.Config
import Test.Tasty.AutoCollect.Constants
import Test.Tasty.AutoCollect.Error
import Test.Tasty.AutoCollect.ModuleType
import Test.Tasty.AutoCollect.Utils.Text
import qualified Test.Tasty.AutoCollect.Utils.TreeMap as TreeMap

generateMainModule :: AutoCollectConfig -> FilePath -> Text -> IO Text
generateMainModule cfg path originalMain = do
  testModules <- sortOn displayName <$> findTestModules cfg path
  let importLines = map ("import qualified " <>) $ map moduleName testModules
      tests = generateTests cfg testModules
  pure $
    if cfgCustomMain cfg
      then rewriteMain importLines tests originalMain
      else mkMainModule cfg path importLines tests

rewriteMain :: [Text] -> Text -> Text -> Text
rewriteMain importLines tests =
  Text.replace "{- AUTOCOLLECT.MAIN.imports -}" (Text.unlines importLines)
    . Text.replace "{- AUTOCOLLECT.MAIN.tests -}" tests

mkMainModule :: AutoCollectConfig -> FilePath -> [Text] -> Text -> Text
mkMainModule AutoCollectConfig{..} path importLines tests =
  Text.unlines
    [ "{-# OPTIONS_GHC -w #-}"
    , ""
    , "module Main (main) where"
    , ""
    , "import Test.Tasty"
    , Text.unlines $ importLines ++ map ("import qualified " <>) ingredientsModules
    , ""
    , "main :: IO ()"
    , "main = defaultMainWithIngredients ingredients (testGroup suiteName tests)"
    , "  where"
    , "    ingredients = " <> ingredients
    , "    suiteName = " <> suiteName
    , "    tests = " <> tests
    ]
  where
    ingredients =
      Text.unwords
        [ listify cfgIngredients
        , "++"
        , if cfgIngredientsOverride then "[]" else "defaultIngredients"
        ]

    ingredientsModules =
      flip map cfgIngredients $ \ingredient ->
        case fst $ Text.breakOnEnd "." ingredient of
          "" -> autocollectError $ "Ingredient needs to be fully qualified: " <> Text.unpack ingredient
          -- remove trailing "."
          s -> Text.init s

    suiteName = quoted $ fromMaybe (Text.pack path) cfgSuiteName

data TestModule = TestModule
  { moduleName :: Text
  -- ^ e.g. "My.Module.Test1"
  , displayName :: Text
  -- ^ The module name to display
  }

-- |
-- Find all test modules using the given path to the Main module.
--
-- >>> findTestModules "test/Main.hs"
-- ["My.Module.Test1", "My.Module.Test2", ...]
findTestModules :: AutoCollectConfig -> FilePath -> IO [TestModule]
findTestModules cfg path = listDirectoryRecursive testDir >>= mapMaybeM toTestModule
  where
    testDir = takeDirectory path

    toTestModule fp = do
      fileContentsBS <- ByteString.readFile fp
      return $
        case (splitExtensions fp, parseModuleType <$> Text.decodeUtf8' fileContentsBS) of
          ((fpNoExt, ".hs"), Right (Just ModuleTest)) ->
            let moduleName = Text.replace "/" "." . Text.pack . makeRelative testDir $ fpNoExt
             in Just
                  TestModule
                    { moduleName
                    , displayName = withoutSuffix (cfgStripSuffix cfg) moduleName
                    }
          _ -> Nothing

    mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
    mapMaybeM f = fmap catMaybes . mapM f

generateTests :: AutoCollectConfig -> [TestModule] -> Text
generateTests AutoCollectConfig{..} testModules =
  case cfgGroupType of
    AutoCollectGroupFlat ->
      -- concat
      --   [ My.Module.Test1.tests
      --   , My.Module.Test2.tests
      --   , ...
      --   ]
      "concat " <> listify (map (addTestList . moduleName) testModules)
    AutoCollectGroupModules ->
      -- [ testGroup "My.Module.Test1" My.Module.Test1.tests
      -- , testGroup "My.Module.Test2" My.Module.Test2.tests
      -- ]
      listify . flip map testModules $ \TestModule{..} ->
        Text.unwords ["testGroup", quoted displayName, addTestList moduleName]
    AutoCollectGroupTree ->
      -- [ testGroup "My"
      --     [ testGroup "Module"
      --         [ testGroup "Test1" My.Module.Test1.tests
      --         , testGroup "Test2" My.Module.Test2.tests
      --         ]
      --     ]
      -- ]
      let getInfo TestModule{..} = (Text.splitOn "." displayName, addTestList moduleName)
       in TreeMap.foldTreeMap testGroupFromTree . TreeMap.fromList . map getInfo $ testModules
  where
    addTestList moduleName = moduleName <> "." <> Text.pack testListIdentifier
    testGroupFromTree mTestsIdentifier subTrees =
      let subGroups =
            flip map (Map.toAscList subTrees) $ \(testModuleDisplay, subTests) ->
              Text.unwords ["testGroup", quoted testModuleDisplay, "$", subTests]
       in case (subGroups, mTestsIdentifier) of
            (subGroups', Nothing) -> listify subGroups'
            ([], Just testsIdentifier) -> testsIdentifier
            (subGroups', Just testsIdentifier) -> "concat " <> listify [testsIdentifier, listify subGroups']

{----- Helpers -----}

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fp = fmap concat . mapM (go . (fp </>)) =<< listDirectory fp
  where
    go child = do
      isDir <- doesDirectoryExist child
      if isDir
        then listDirectoryRecursive child
        else pure [child]
