{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.Config (
  AutoCollectConfig (..),
  AutoCollectGroupType (..),
  defaultConfig,
  parseConfig,
) where

import Data.Text (Text)
import qualified Data.Text as Text

{----- Configuration -----}

-- | Configuration for generating the Main module, specified as a block comment.
data AutoCollectConfig = AutoCollectConfig
  { cfgSuiteName :: Maybe String
  -- ^ The name of the entire test suite
  , cfgGroupType :: AutoCollectGroupType
  -- ^ How tests should be grouped (defaults to "modules")
  , cfgIngredients :: [String]
  -- ^ A comma-separated list of extra tasty ingredients to include
  , cfgIngredientsOverride :: Bool
  -- ^ If true, 'cfgIngredients' overrides the default tasty ingredients;
  -- otherwise, they're prepended to the list of default ingredients (defaults to false)
  , cfgStripSuffix :: String
  -- ^ The suffix to strip from a test, e.g. @strip_suffix = Test@ will relabel
  -- a module @Foo.BarTest@ to @Foo.Bar@.
  }

data AutoCollectGroupType
  = -- | All tests will be flattened like
    --
    -- @
    -- test1
    -- test2
    -- test3
    -- @
    AutoCollectGroupFlat
  | -- | Tests will be grouped by module
    --
    -- @
    -- MyModule.MyTest1
    --   test1
    --   test2
    -- MyModule.MyTest2
    --   test3
    -- @
    AutoCollectGroupModules
  | -- | Test modules will be grouped as a tree
    --
    -- @
    -- MyModule
    --   MyTest1
    --     test1
    --     test2
    --   MyTest2
    --     test3
    -- @
    AutoCollectGroupTree

defaultConfig :: AutoCollectConfig
defaultConfig =
  AutoCollectConfig
    { cfgSuiteName = Nothing
    , cfgGroupType = AutoCollectGroupModules
    , cfgIngredients = []
    , cfgIngredientsOverride = False
    , cfgStripSuffix = ""
    }

parseConfig :: String -> Either String AutoCollectConfig
parseConfig = fmap resolve . mapM parseLine . filter (not . isIgnoredLine) . Text.lines . Text.pack
  where
    isIgnoredLine s = Text.null (Text.strip s) || ("#" `Text.isPrefixOf` s)

    parseLine :: Text -> Either String (AutoCollectConfig -> AutoCollectConfig)
    parseLine s = do
      (k, v) <-
        case Text.splitOn "=" s of
          [k, v] -> pure (Text.strip k, Text.strip v)
          _ -> Left $ "Invalid configuration line: " ++ show s

      case k of
        "suite_name" ->
          pure $ \cfg -> cfg{cfgSuiteName = Just $ Text.unpack v}
        "group_type" -> do
          groupType <- parseGroupType v
          pure $ \cfg -> cfg{cfgGroupType = groupType}
        "ingredients" -> do
          let ingredients = map (Text.unpack . Text.strip) . Text.splitOn "," $ v
          pure $ \cfg -> cfg{cfgIngredients = ingredients}
        "ingredients_override" -> do
          override <- parseBool v
          pure $ \cfg -> cfg{cfgIngredientsOverride = override}
        "strip_suffix" ->
          pure $ \cfg -> cfg{cfgStripSuffix = Text.unpack v}
        _ -> Left $ "Invalid configuration key: " ++ show k

    resolve fs = compose fs defaultConfig

parseGroupType :: Text -> Either String AutoCollectGroupType
parseGroupType = \case
  "flat" -> pure AutoCollectGroupFlat
  "modules" -> pure AutoCollectGroupModules
  "tree" -> pure AutoCollectGroupTree
  ty -> Left $ "Invalid group_type: " ++ show ty

parseBool :: Text -> Either String Bool
parseBool s =
  case Text.toLower s of
    "true" -> pure True
    "false" -> pure False
    _ -> Left $ "Invalid bool: " ++ show s

{----- Utilities -----}

-- | [f, g, h] => (h . g . f)
compose :: [a -> a] -> a -> a
compose fs = foldr (\f acc -> acc . f) id fs
