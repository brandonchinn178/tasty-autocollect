{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.Constants (
  testListIdentifier,
  testIdentifier,
  isMainComment,
  isTestComment,
  isTestExportComment,
) where

import Data.Char (toLower)
import qualified Data.Text as Text

import Test.Tasty.AutoCollect.Utils.Text

testListIdentifier :: String
testListIdentifier = "tasty_autocollect_tests"

testIdentifier :: Int -> String
testIdentifier x = "tasty_autocollect_test_" ++ show x

isMainComment :: String -> Bool
isMainComment = matches "autocollect.main"

isTestComment :: String -> Bool
isTestComment = matches "autocollect.test"

isTestExportComment :: String -> Bool
isTestExportComment = matches "autocollect.test.export" . unwrap
  where
    -- Support '{- $autocollect.test.export$ -}' for Ormolu/Fourmolu support
    -- Remove when dropping support for Fourmolu < 0.13, Ormolu < 0.7
    unwrap = Text.unpack . withoutPrefix "$" . withoutSuffix "$" . Text.pack

matches :: String -> String -> Bool
matches label s = map toLower s == label
