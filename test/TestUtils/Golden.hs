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

testGolden :: String -> FilePath -> IO Text -> TestTree
testGolden name fp = goldenVsString name ("test/golden/" ++ fp) . fmap (TextL.encodeUtf8 . TextL.fromStrict)

testGoldenVersioned :: String -> FilePath -> IO Text -> TestTree
testGoldenVersioned name fp = testGolden name fp'
  where
    fp' =
      case unsafePerformIO (lookupEnv "TEST_PREFER_OLDEST") of
        Just "1" -> fp <> ".prefer-oldest"
        _ -> fp
