module Test.Tasty.AutoCollect.Constants (
  testListIdentifier,
  testIdentifier,
  isMainComment,
  isTestComment,
  isTestExportComment,
) where

import Data.Char (toLower)

testListIdentifier :: String
testListIdentifier = "tasty_autocollect_tests"

testIdentifier :: Int -> String
testIdentifier x = "tasty_autocollect_test_" ++ show x

isMainComment :: String -> Bool
isMainComment = matches "autocollect.main"

isTestComment :: String -> Bool
isTestComment = matches "autocollect.test"

isTestExportComment :: String -> Bool
isTestExportComment = matches "autocollect.test.export"

matches :: String -> String -> Bool
matches label s = map toLower s == label
