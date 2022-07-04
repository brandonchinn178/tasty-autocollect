{- AUTOCOLLECT.TEST -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.ConvertTestTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import Data.Maybe (maybeToList)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import TestUtils.Integration

test_batch :: [TestTree]
test_batch =
  [ testCase ("plugin works when " ++ mkLabel ext) $
    assertSuccess_ $
      runTestWith
        (\proj -> proj{extraGhcArgs = maybeToList ext <> extraGhcArgs proj})
        [ "test_testCase :: Assertion"
        , "test_testCase \"1 = 1\" = 1 @?= 1"
        ]
  | ext <-
      [ Just "-XOverloadedStrings"
      , Just "-XOverloadedLists"
      , Nothing
      ]
  ]
  where
    mkLabel = \case
      Nothing -> "no extensions are enabled"
      Just ext -> "enabling " <> Text.unpack ext

test_todo :: ()
test_todo "Integration: ConvertTest literal ints/floats" = ()

test_todo :: ()
test_todo "Integration: error without tasty package" = ()

test_todo :: ()
test_todo "Integration: arbitrary test helpers" = ()

test_todo :: ()
test_todo "Integration: different arities" = ()

test_todo :: ()
test_todo "Integration: works when exporting everything" = ()

test_todo :: ()
test_todo "Integration: works when file contains multi-function signature" = ()

test_todo :: ()
test_todo "Integration: test_batch works" = ()

test_todo :: ()
test_todo "Integration: test_batch fails with args" = ()

test_todo :: ()
test_todo "Integration: test_batch fails with different type" = ()
