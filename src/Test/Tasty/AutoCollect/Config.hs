{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Tasty.AutoCollect.Config (
  AutoCollectConfig' (..),
  AutoCollectConfig,
  AutoCollectConfigPartial,
  AutoCollectGroupType (..),
  parseConfig,
  resolveConfig,
) where

import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

{----- Configuration -----}

type family Apply f a where
  Apply Maybe a = Maybe a
  Apply Identity a = a

-- | Configuration for generating the Main module, specified as a block comment.
data AutoCollectConfig' f = AutoCollectConfig
  { cfgSuiteName :: Apply f (Maybe Text)
  -- ^ The name of the entire test suite
  , cfgGroupType :: Apply f AutoCollectGroupType
  -- ^ How tests should be grouped (defaults to "modules")
  , cfgStripSuffix :: Apply f Text
  -- ^ The suffix to strip from a test, e.g. @strip_suffix = Test@ will relabel
  -- a module @Foo.BarTest@ to @Foo.Bar@.
  , cfgIngredients :: Apply f [Text]
  -- ^ A comma-separated list of extra tasty ingredients to include
  , cfgIngredientsOverride :: Apply f Bool
  -- ^ If true, 'cfgIngredients' overrides the default tasty ingredients;
  -- otherwise, they're prepended to the list of default ingredients (defaults to false)
  , cfgCustomMain :: Apply f Bool
  }

type AutoCollectConfigPartial = AutoCollectConfig' Maybe
deriving instance Show AutoCollectConfigPartial
deriving instance Eq AutoCollectConfigPartial

type AutoCollectConfig = AutoCollectConfig' Identity
deriving instance Show AutoCollectConfig
deriving instance Eq AutoCollectConfig

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

-- | Config on RHS overrides config on LHS.
instance Semigroup AutoCollectConfigPartial where
  cfg1 <> cfg2 =
    AutoCollectConfig
      { cfgSuiteName = cfgSuiteName cfg2 <|> cfgSuiteName cfg1
      , cfgGroupType = cfgGroupType cfg2 <|> cfgGroupType cfg1
      , cfgIngredients = cfgIngredients cfg2 <|> cfgIngredients cfg1
      , cfgIngredientsOverride = cfgIngredientsOverride cfg2 <|> cfgIngredientsOverride cfg1
      , cfgStripSuffix = cfgStripSuffix cfg2 <|> cfgStripSuffix cfg1
      , cfgCustomMain = cfgCustomMain cfg2 <|> cfgCustomMain cfg1
      }

instance Monoid AutoCollectConfigPartial where
  mempty =
    AutoCollectConfig
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing

{----- Parsing -----}

parseConfig :: Text -> Either Text AutoCollectConfigPartial
parseConfig = fmap mconcat . mapM parseLine . filter (not . isIgnoredLine) . Text.lines
  where
    isIgnoredLine s = Text.null (Text.strip s) || ("#" `Text.isPrefixOf` s)

    parseLine :: Text -> Either Text AutoCollectConfigPartial
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
          pure mempty{cfgSuiteName = Just (Just v)}
        "group_type" -> do
          groupType <- parseGroupType v
          pure mempty{cfgGroupType = Just groupType}
        "strip_suffix" ->
          pure mempty{cfgStripSuffix = Just v}
        "ingredients" -> do
          let ingredients = map Text.strip . Text.splitOn "," $ v
          pure mempty{cfgIngredients = Just ingredients}
        "ingredients_override" -> do
          override <- parseBool v
          pure mempty{cfgIngredientsOverride = Just override}
        "custom_main" -> do
          customMain <- parseBool v
          pure mempty{cfgCustomMain = Just customMain}
        _ -> Left $ "Invalid configuration key: " <> Text.pack (show k)

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

{----- Resolving -----}

resolveConfig :: AutoCollectConfigPartial -> IO AutoCollectConfig
resolveConfig = pure . resolve
  where
    resolve :: AutoCollectConfigPartial -> AutoCollectConfig
    resolve AutoCollectConfig{..} =
      AutoCollectConfig
        { cfgSuiteName = fromMaybe Nothing cfgSuiteName
        , cfgGroupType = fromMaybe AutoCollectGroupModules cfgGroupType
        , cfgIngredients = fromMaybe [] cfgIngredients
        , cfgIngredientsOverride = fromMaybe False cfgIngredientsOverride
        , cfgStripSuffix = fromMaybe "" cfgStripSuffix
        , cfgCustomMain = fromMaybe False cfgCustomMain
        }
