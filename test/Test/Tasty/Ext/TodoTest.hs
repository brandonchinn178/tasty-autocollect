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

test = testCase "TODO tests appear as successful tests" $ do
  (stdout, _) <-
    assertSuccess $
      runTest
        [ "test_todo = \"a skipped test\""
        ]
  getTestLines stdout @?~ containsStripped (eq "a skipped test: TODO")

test = testCase "TODO tests can wrap any type" $
  assertSuccess_ $
    runTest
      [ "test_todo = \"todo with int\""
      , "test_todo = \"todo with bool\""
      ]

test = testCase "TODO tests show compilation errors" $ do
  (_, stderr) <-
    assertAnyFailure $
      runTest
        [ "test_todo = \"partially implemented todo\""
        ]
  getTestLines stderr @?~ containsStripped (eq "• Couldn't match expected type ‘Int’ with actual type ‘Bool’")
