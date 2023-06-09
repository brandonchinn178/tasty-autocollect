{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.Ext.TodoTest (
{- AUTOCOLLECT.TEST.export -}

) where

import Test.Predicates
import Test.Predicates.HUnit
import Test.Tasty.HUnit

import TestUtils.Golden
import TestUtils.Integration
import TestUtils.Predicates

test = testCase "TODO tests appear as successful tests" $ do
  (stdout, _) <- assertSuccess $ runTest ["test_todo = \"a skipped test\""]
  getTestLines stdout @?~ containsStripped (eq "a skipped test: TODO")

test =
  testCase "test_todo may specify type" $
    assertSuccess_ . runTest $
      [ "test_todo :: String"
      , "test_todo = \"a pending test\""
      ]

test = testGolden "test_todo fails when given arguments" "test_todo_args.golden" $ do
  (_, stderr) <-
    assertAnyFailure . runTest $
      [ "test_todo \"some name\" = \"a pending test\""
      ]
  pure stderr

test = testGolden "test_todo fails when specifying wrong type" "test_todo_type.golden" $ do
  (_, stderr) <-
    assertAnyFailure . runTest $
      [ "test_todo :: Int"
      , "test_todo = \"a pending test\""
      ]
  pure stderr

test =
  testGolden "--fail-todos makes TODO tests fail" "fail_todos.golden" $ do
    (stdout, _) <-
      assertAnyFailure $
        runTestWith
          (\proj -> proj{runArgs = ["--fail-todos"]})
          ["test_todo = \"a pending test\""]
    pure $ normalizeTestOutput stdout
