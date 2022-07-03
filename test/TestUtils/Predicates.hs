{-# LANGUAGE TemplateHaskell #-}

module TestUtils.Predicates (
  strippedEq,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Test.Predicates

strippedEq :: Text -> Predicate Text
strippedEq = $(qWith [|Text.strip|]) . eq
