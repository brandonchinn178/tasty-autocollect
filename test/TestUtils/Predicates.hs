{-# LANGUAGE TemplateHaskell #-}

module TestUtils.Predicates (
  containsStrippedLine,
) where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Predicates

{- |
A predicate for checking that the given text contains
a line that, when stripped of whitespace, satisfies the
given predicate.

Writing our own because explainable-predicate's `contain`
function doesn't provide a good output on failure.
-}
containsStrippedLine :: Predicate Text -> Predicate Text
containsStrippedLine p =
  Predicate
    { showPredicate = "contains a stripped line matching: " ++ showPredicate p
    , showNegation = "does not contain a stripped line matching: " ++ showNegation p
    , accept = any (accept p . Text.strip) . Text.lines
    , explain = \xs ->
        case filter (accept p . Text.strip . snd) $ zip [1 ..] (Text.lines xs) of
          [] -> "No lines match: " ++ showPredicate p ++ "\n" ++ show (Text.lines xs)
          matched ->
            intercalate "\n" $
              [ "element #" ++ show (i :: Int) ++ ": " ++ explain p x
              | (i, x) <- matched
              ]
    }
