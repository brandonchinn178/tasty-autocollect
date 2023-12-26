{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestUtils.QuickCheck (
  PrintableText (..),
  genMixedCase,
) where

import Data.Char (toLower, toUpper)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty.QuickCheck

newtype PrintableText = PrintableText {getPrintableText :: Text}
  deriving (Show, IsString)

instance Arbitrary PrintableText where
  arbitrary = PrintableText . Text.pack . getPrintableString <$> arbitrary

genMixedCase :: Text -> Gen Text
genMixedCase s = withCases <$> infiniteList
  where
    withCases cases = Text.pack . zipWith setCase cases . Text.unpack $ s

    setCase False = toLower
    setCase True = toUpper
