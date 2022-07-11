module Test.Tasty.AutoCollect.Error (
  autocollectError,
) where

import Test.Tasty.AutoCollect.GHC

autocollectError :: String -> a
autocollectError msg =
  pgmError . unlines $
    [ ""
    , "******************** tasty-autocollect failure ********************"
    , msg
    ]
