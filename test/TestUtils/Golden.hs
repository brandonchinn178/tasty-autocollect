{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TestUtils.Golden (
  testGolden,
) where

import Data.Text (Text)
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import Test.Tasty (TestTree)
import Test.Tasty.Golden

#if __GLASGOW_HASKELL__ != 904
import qualified Data.Text as Text
#endif

testGolden :: String -> FilePath -> IO Text -> TestTree
testGolden name fp = goldenVsString name ("test/golden/" ++ fp) . fmap (TextL.encodeUtf8 . TextL.fromStrict . normalizePgmError)

-- GHC 9.4 added a prefix to pgmError messages. Normalize old versions to show new format.
normalizePgmError :: Text -> Text
#if __GLASGOW_HASKELL__ == 904
normalizePgmError = id
#else
normalizePgmError t
  | "tasty-autocollect failure" `Text.isInfixOf` t =  "\n<no location info>: error:\n    " <> t
  | otherwise = t
#endif
