{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.Ext.TodoTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import Test.Predicates
import Test.Predicates.HUnit
import Test.Tasty.HUnit

import TestUtils.Integration
import TestUtils.Predicates

test_testCase :: Assertion
test_testCase "TODO tests appear as successful tests" = do
  (stdout, _) <-
    assertSuccess $
      runTest
        [ "test_todo :: ()"
        , "test_todo \"a skipped test\" = ()"
        ]
  stdout @?~ containsStrippedLine (eq "a skipped test: TODO")

test_testCase :: Assertion
test_testCase "TODO tests can wrap any type" =
  assertSuccess_ $
    runTest
      [ "test_todo :: Int"
      , "test_todo \"todo with int\" = 1"
      , "test_todo :: Bool"
      , "test_todo \"todo with bool\" = True"
      ]

test_testCase :: Assertion
test_testCase "TODO tests show compilation errors" = do
  (_, stderr) <-
    assertAnyFailure $
      runTest
        [ "test_todo :: Assertion"
        , "test_todo \"partially implemented todo\" = length [] @?= True"
        ]
  stderr @?~ containsStrippedLine (eq "• Couldn't match expected type ‘Int’ with actual type ‘Bool’")
