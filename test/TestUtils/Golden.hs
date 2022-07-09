{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestUtils.Golden (
  testGolden,
) where

import Data.Text (Text)
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import Test.Tasty (TestTree)
import Test.Tasty.Golden

testGolden :: String -> FilePath -> IO Text -> TestTree
testGolden name fp = goldenVsString name ("test/golden/" ++ fp) . fmap (TextL.encodeUtf8 . TextL.fromStrict)
