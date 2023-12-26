{-# LANGUAGE TemplateHaskell #-}

module TestUtils.Predicates (
  containsStripped,
) where

import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Predicates

-- | A predicate for checking that the given lines of text
-- contain a line that, when stripped of whitespace, satisfies
-- the given predicate.
--
-- Writing our own because explainable-predicate's `contain`
-- function doesn't provide a good output on failure.
containsStripped :: Predicate Text -> Predicate [Text]
containsStripped p =
  Predicate
    { showPredicate = "contains a stripped line matching: " ++ showPredicate p
    , showNegation = "does not contain a stripped line matching: " ++ showNegation p
    , accept = any (accept p . Text.strip)
    , explain = \xs ->
        case filter (accept p . Text.strip . snd) $ zip [1 ..] xs of
          [] -> "No lines match: " ++ showPredicate p ++ "\n" ++ show xs
          matched ->
            intercalate "\n" $
              [ "element #" ++ show (i :: Int) ++ ": " ++ explain p x
              | (i, x) <- matched
              ]
    }
