module Test.Tasty.AutoCollect.Error (
  autocollectError,
) where

import Data.List (dropWhileEnd)

import Test.Tasty.AutoCollect.GHC

autocollectError :: String -> a
autocollectError msg =
  pgmError . dropWhileEnd (== '\n') . unlines $
    [ ""
    , "******************** tasty-autocollect failure ********************"
    , msg
    ]
