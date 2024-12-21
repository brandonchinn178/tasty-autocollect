{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TestUtils.Golden (
  testGolden,
  testGoldenVersioned,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding qualified as TextL
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree)
import Test.Tasty.Golden

testGolden :: String -> FilePath -> IO Text -> TestTree
testGolden name fp = goldenVsString name ("test/golden/" ++ fp) . fmap encodeText
  where
    encodeText = TextL.encodeUtf8 . TextL.fromStrict . normalize
    normalize t
      | "tasty-autocollect failure" `Text.isInfixOf` t = normalizeAutocollectError t
      | otherwise = t

testGoldenVersioned :: String -> FilePath -> IO Text -> TestTree
testGoldenVersioned name fp = testGolden name fp'
  where
    fp' =
      case unsafePerformIO (lookupEnv "TEST_PREFER_OLDEST") of
        Just "1" -> fp <> ".prefer-oldest"
        _ -> fp

normalizeAutocollectError :: Text -> Text
normalizeAutocollectError
  | __GLASGOW_HASKELL__ < (910 :: Int) =
      -- GHC < 9.10 had an extra newline at the beginning + one fewer newline at the end
      (<> "\n") . stripLeadingNewline
  | otherwise = id
  where
    stripLeadingNewline t =
      case Text.uncons t of
        Just ('\n', t') -> t'
        _ -> t
