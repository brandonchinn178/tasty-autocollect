module TestUtils.Assertion (
  hasLineContaining,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty.HUnit

{- |
@text `hasLineContaining` sub@ succeeds if the given text contains a
newline-delimited line containing the given substring.
-}
hasLineContaining :: Text -> Text -> Assertion
hasLineContaining text sub =
  assertBool errorMessage $
    any (sub `Text.isInfixOf`) (Text.lines text)
  where
    errorMessage =
      unlines
        [ "Could not find line matching:"
        , Text.unpack sub
        , "In text:"
        , Text.unpack text
        ]
