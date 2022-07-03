{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestUtils.QuickCheck (
  PrintableText (..),
) where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty.QuickCheck

newtype PrintableText = PrintableText {getPrintableText :: Text}
  deriving (Show, IsString)

instance Arbitrary PrintableText where
  arbitrary = PrintableText . Text.pack . getPrintableString <$> arbitrary
