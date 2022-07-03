{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.Ext.TodoTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import qualified Data.Text as Text
import Test.Predicates
import Test.Predicates.HUnit
import Test.Tasty.HUnit

import TestUtils.Integration
import TestUtils.Predicates

test_testCase :: Assertion
test_testCase "TODO tests appear as successful tests" = do
  (stdout, _) <-
    runTest_
      [ "test_todo :: ()"
      , "test_todo \"a skipped test\" = ()"
      ]
  Text.lines stdout @?~ contains (strippedEq "a skipped test: TODO")

test_testCase :: Assertion
test_testCase "TODO tests can wrap any type" = do
  (code, _, _) <-
    runTest
      [ "test_todo :: Int"
      , "test_todo \"todo with int\" = 1"
      , "test_todo :: Bool"
      , "test_todo \"todo with bool\" = True"
      ]
  code @?= ExitSuccess

test_testCase :: Assertion
test_testCase "TODO tests show compilation errors" = do
  (code, _, stderr) <-
    runTest
      [ "test_todo :: Assertion"
      , "test_todo \"partially implemented todo\" = length [] @?= True"
      ]
  code @?= ExitFailure 1
  Text.lines stderr @?~ contains (strippedEq "• Couldn't match expected type ‘Int’ with actual type ‘Bool’")
