module Test.Tasty.AutoCollect.Error (
  autocollectError,
) where

import GHC.Plugins

autocollectError :: String -> a
autocollectError msg =
  pgmError . unlines $
    [ ""
    , "******************** tasty-autocollect failure ********************"
    , msg
    , ""
    , "Please report this as a bug at: https://github.com/brandonchinn178/tasty-autocollect/issues/"
    ]
