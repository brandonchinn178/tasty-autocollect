{- AUTOCOLLECT.TEST -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Tasty.AutoCollect.ConfigTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import Control.Monad (forM_)
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Predicates
import Test.Predicates.HUnit
import Test.Predicates.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Tasty.AutoCollect.Config
import TestUtils.QuickCheck

{----- Configuration syntax -----}

test =
  testCase "parseConfig ignores comments" $
    parseConfig "# this is a comment" @?~ right anything

test =
  testCase "parseConfig ignores empty lines" $
    parseConfig "\n\n\n" @?~ right anything

test_prop "parseConfig errors on ill-formed lines" =
  forAll (invalidLine `suchThat` (not . isIgnored)) $ \line ->
    parseConfig line `satisfies` left (startsWith "Invalid configuration line:")
  where
    isIgnored line = Text.all isSpace line || ("#" `Text.isPrefixOf` line)
    linePart = do
      ConfigPiece s <- arbitrary
      spaces <- arbitrary
      pure $ wrapSpaces spaces s

    invalidLine =
      oneof
        [ -- no '=' at all
          linePart
        , -- '... ='
          (<> "=") <$> linePart
        , -- '= ...'
          ("=" <>) <$> linePart
        , -- multiple '=' signs
          do
            Positive n <- arbitrary
            Text.intercalate "=" <$> vectorOf (2 + n) linePart
        ]

test_prop :: ConfigPiece -> Spaces -> Spaces -> Property
test_prop "parseConfig strips whitespace" (ConfigPiece v) kspaces vspaces =
  let k' = wrapSpaces kspaces k
      v' = wrapSpaces vspaces v
   in parseConfig (k' <> "=" <> v') === parseConfig (k <> "=" <> v)
  where
    k = "suite_name"

{----- Configuration options -----}

test =
  testCase "parseConfig parses import" $
    parseConfig "import = foo.conf, ../bar/baz.conf"
      @?~ right (cfgImports `with` just (eq ["foo.conf", "../bar/baz.conf"]))

test_prop :: ConfigPiece -> Property
test_prop "parseConfig parses suite_name" (ConfigPiece v) =
  parseConfig ("suite_name = " <> v)
    `satisfies` right (cfgSuiteName `with` just (just (eq v)))

test_prop :: Property
test_prop "parseConfig parses group_type" =
  forAll (elements groupTypeOptions) $ \(groupTypeName, groupType) ->
    parseConfig ("group_type = " <> groupTypeName)
      `satisfies` right (cfgGroupType `with` just (eq groupType))

test_prop :: ConfigPiece -> Property
test_prop "parseConfig errors on invalid group_type" (ConfigPiece v) =
  v `notElem` map fst groupTypeOptions ==>
    parseConfig ("group_type = " <> v) `satisfies` left (startsWith "Invalid group_type:")

groupTypeOptions :: [(Text, AutoCollectGroupType)]
groupTypeOptions =
  [ ("flat", AutoCollectGroupFlat)
  , ("modules", AutoCollectGroupModules)
  , ("tree", AutoCollectGroupTree)
  ]

test_prop :: ConfigPiece -> Property
test_prop "parseConfig parses strip_suffix" (ConfigPiece v) =
  parseConfig ("strip_suffix = " <> v)
    `satisfies` right (cfgStripSuffix `with` just (eq v))

test_prop :: NonEmptyList HsIdentifier -> Property
test_prop "parseConfig parses ingredients" (NonEmpty (map getHsIdentifier -> ingredients)) =
  parseConfig ("ingredients = " <> Text.intercalate "," ingredients)
    `satisfies` right (cfgIngredients `with` just (eq ingredients))

test_prop :: NonEmptyList (HsIdentifier, Spaces) -> Property
test_prop "parseConfig strips whitespace when parsing ingredients" (NonEmpty (map (first getHsIdentifier) -> identifiers)) =
  let ingredientsVal = Text.intercalate "," . map (\(s, spaces) -> wrapSpaces spaces s) $ identifiers
      ingredients = map fst identifiers
   in parseConfig ("ingredients = " <> ingredientsVal)
        `satisfies` right (cfgIngredients `with` just (eq ingredients))

test_prop :: BoolOption -> Property
test_prop "parseConfig parses ingredients_override (case insensitive)" option =
  parseConfig ("ingredients_override = " <> getText option)
    `satisfies` right (cfgIngredientsOverride `with` just (eq (getBool option)))

test_prop :: ConfigPiece -> Property
test_prop "parseConfig errors on invalid ingredients_override" (ConfigPiece v) =
  Text.toLower v `notElem` ["true", "false"] ==>
    parseConfig ("ingredients_override = " <> v) `satisfies` left (startsWith "Invalid bool:")

test_prop :: ConfigPiece -> ConfigPiece -> Property
test_prop "parseConfig errors on unknown keys" (ConfigPiece k) (ConfigPiece v) =
  (k `notElem` validKeys) && not ("#" `Text.isPrefixOf` k) ==>
    parseConfig (k <> " = " <> v) `satisfies` left (startsWith "Invalid configuration key:")
  where
    validKeys =
      [ "suite_name"
      , "group_type"
      , "ingredients"
      , "ingredients_override"
      , "strip_suffix"
      ]

{----- Configuration resolution -----}

test =
  testCase "resolveConfig imports config recursively" $
    withSystemTempDirectory "tasty-autocollect-resolveConfig" $ \tmpdir -> do
      forM_ files $ \(fpRel, fileLines) -> do
        let fp = tmpdir </> fpRel
        createDirectoryIfMissing True (takeDirectory fp)
        Text.writeFile fp (Text.unlines fileLines)
      cfg <- resolveConfig (tmpdir </> "Main.hs") config
      cfgSuiteName cfg @?= Just "foo"
      cfgIngredients cfg @?= ["baseIngredients"]
  where
    files =
      [
        ( "foo/autocollect.conf"
        ,
          [ "import = ../base/autocollect.conf"
          , "suite_name = foo"
          ]
        )
      ,
        ( "base/autocollect.conf"
        ,
          [ "suite_name = base"
          , "ingredients = baseIngredients"
          ]
        )
      ]
    config =
      (mempty :: AutoCollectConfigPartial)
        { cfgImports = Just ["foo/autocollect.conf"]
        }

{----- Helpers -----}

-- | A Text suitable for use as a key or value in the configuration.
--
-- Specifically, will be a non-empty string that does not contain '=',
-- newlines, or trailing/leading spaces.
newtype ConfigPiece = ConfigPiece {getConfigPiece :: Text}
  deriving (Show)

instance Arbitrary ConfigPiece where
  arbitrary =
    fmap ConfigPiece $
      (`suchThat` not . Text.null) $
        Text.strip . Text.filter (`notElem` ['=', '\n']) . getPrintableText <$> arbitrary

data Spaces = Spaces {before :: Int, after :: Int}
  deriving (Show)

wrapSpaces :: Spaces -> Text -> Text
wrapSpaces Spaces{..} s =
  Text.replicate before " " <> s <> Text.replicate after " "

instance Arbitrary Spaces where
  arbitrary = do
    Positive before <- arbitraryNumSpaces
    Positive after <- arbitraryNumSpaces
    pure $ Spaces before after
    where
      arbitraryNumSpaces =
        frequency
          [ (10, pure $ Positive 0)
          , (1, arbitrary)
          ]

-- | An arbitrary Haskell identifier
newtype HsIdentifier = HsIdentifier {getHsIdentifier :: Text}
  deriving (Show)

instance Arbitrary HsIdentifier where
  arbitrary = do
    modules <-
      frequency
        [ (5, pure [])
        , (1, listOf (identStartingWith large))
        ]
    ident <- identStartingWith small
    pure $ HsIdentifier $ Text.intercalate "." $ modules ++ [ident]
    where
      small = ['a' .. 'z']
      large = ['A' .. 'Z']
      digit = ['0' .. '9']

      identStartingWith start = do
        c <- elements start
        cs <- listOf $ elements (small ++ large ++ digit)
        pure $ Text.pack (c : cs)

data BoolOption = BoolOption {getBool :: Bool, getText :: Text}
  deriving (Show)

instance Arbitrary BoolOption where
  arbitrary = do
    b <- arbitrary
    s <- genMixedCase (Text.pack $ show b)
    pure $ BoolOption b s
