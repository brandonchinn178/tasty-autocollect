{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

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
  { cfgSuiteName :: Maybe Text
  -- ^ The name of the entire test suite
  , cfgGroupType :: AutoCollectGroupType
  -- ^ How tests should be grouped (defaults to "modules")
  , cfgIngredients :: [Text]
  -- ^ A comma-separated list of extra tasty ingredients to include
  , cfgIngredientsOverride :: Bool
  -- ^ If true, 'cfgIngredients' overrides the default tasty ingredients;
  -- otherwise, they're prepended to the list of default ingredients (defaults to false)
  , cfgStripSuffix :: Text
  -- ^ The suffix to strip from a test, e.g. @strip_suffix = Test@ will relabel
  -- a module @Foo.BarTest@ to @Foo.Bar@.
  }
  deriving (Show, Eq)

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
  deriving (Show, Eq)

defaultConfig :: AutoCollectConfig
defaultConfig =
  AutoCollectConfig
    { cfgSuiteName = Nothing
    , cfgGroupType = AutoCollectGroupModules
    , cfgIngredients = []
    , cfgIngredientsOverride = False
    , cfgStripSuffix = ""
    }

parseConfig :: Text -> Either Text AutoCollectConfig
parseConfig = fmap resolve . mapM parseLine . filter (not . isIgnoredLine) . Text.lines
  where
    isIgnoredLine s = Text.null (Text.strip s) || ("#" `Text.isPrefixOf` s)

    parseLine :: Text -> Either Text (AutoCollectConfig -> AutoCollectConfig)
    parseLine s = do
      (k, v) <-
        case Text.splitOn "=" s of
          [Text.strip -> k, Text.strip -> v]
            | not (Text.null k)
            , not (Text.null v) ->
                pure (k, v)
          _ -> Left $ "Invalid configuration line: " <> Text.pack (show s)

      case k of
        "suite_name" ->
          pure $ \cfg -> cfg{cfgSuiteName = Just v}
        "group_type" -> do
          groupType <- parseGroupType v
          pure $ \cfg -> cfg{cfgGroupType = groupType}
        "ingredients" -> do
          let ingredients = map Text.strip . Text.splitOn "," $ v
          pure $ \cfg -> cfg{cfgIngredients = ingredients}
        "ingredients_override" -> do
          override <- parseBool v
          pure $ \cfg -> cfg{cfgIngredientsOverride = override}
        "strip_suffix" ->
          pure $ \cfg -> cfg{cfgStripSuffix = v}
        _ -> Left $ "Invalid configuration key: " <> Text.pack (show k)

    resolve fs = compose fs defaultConfig

parseGroupType :: Text -> Either Text AutoCollectGroupType
parseGroupType = \case
  "flat" -> pure AutoCollectGroupFlat
  "modules" -> pure AutoCollectGroupModules
  "tree" -> pure AutoCollectGroupTree
  ty -> Left $ "Invalid group_type: " <> Text.pack (show ty)

parseBool :: Text -> Either Text Bool
parseBool s =
  case Text.toLower s of
    "true" -> pure True
    "false" -> pure False
    _ -> Left $ "Invalid bool: " <> Text.pack (show s)

{----- Utilities -----}

-- | [f, g, h] => (h . g . f)
compose :: [a -> a] -> a -> a
compose fs = foldr (\f acc -> acc . f) id fs
