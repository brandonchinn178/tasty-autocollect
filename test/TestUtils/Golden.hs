{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TestUtils.Golden (
  testGolden,
  testGoldenVersioned,
) where

import Data.Text (Text)
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding qualified as TextL
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree)
import Test.Tasty.Golden

#if __GLASGOW_HASKELL__ < 904
import qualified Data.Text as Text
#endif

testGolden :: String -> FilePath -> IO Text -> TestTree
testGolden name fp = goldenVsString name ("test/golden/" ++ fp) . fmap (TextL.encodeUtf8 . TextL.fromStrict . normalizePgmError)

testGoldenVersioned :: String -> FilePath -> IO Text -> TestTree
testGoldenVersioned name fp = testGolden name fp'
  where
    fp' =
      case unsafePerformIO (lookupEnv "TEST_PREFER_OLDEST") of
        Just "1" -> fp <> ".prefer-oldest"
        _ -> fp

-- GHC 9.4 added a prefix to pgmError messages. Normalize old versions to show new format.
normalizePgmError :: Text -> Text
#if __GLASGOW_HASKELL__ >= 904
normalizePgmError = id
#else
normalizePgmError t
  | "tasty-autocollect failure" `Text.isInfixOf` t =  "\n<no location info>: error:\n    " <> t
  | otherwise = t
#endif
