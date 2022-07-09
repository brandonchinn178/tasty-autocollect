{-# LANGUAGE TemplateHaskell #-}

module TestUtils.Predicates (
  stripped,
  strippedEq,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Test.Predicates

stripped :: Predicate Text -> Predicate Text
stripped = $(qWith [|Text.strip|])

strippedEq :: Text -> Predicate Text
strippedEq = stripped . eq
