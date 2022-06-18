module Test.Tasty.AutoCollect.Constants (
  testListIdentifier,
  testIdentifier,
) where

testListIdentifier :: String
testListIdentifier = "tasty_autocollect_tests"

testIdentifier :: Int -> String
testIdentifier x = "tasty_autocollect_test_" ++ show x
