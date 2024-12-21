{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Ext.Todo (
  testTreeTodo,
) where

import Data.Proxy (Proxy (..))
import Test.Tasty.Options (
  IsOption (..),
  OptionDescription (..),
  flagCLParser,
  lookupOption,
  safeReadBool,
 )
import Test.Tasty.Providers (
  IsTest (..),
  TestName,
  testFailed,
  testPassed,
 )
import Test.Tasty.Runners (Result (..), TestTree (..))

data TodoTest = TodoTest

instance IsTest TodoTest where
  run opts _ _ = pure testResult{resultShortDescription = "TODO"}
    where
      FailTodos shouldFail = lookupOption opts
      testResult =
        if shouldFail
          then testFailed "Failing because --fail-todos was set"
          else testPassed ""

  testOptions = pure [Option (Proxy @FailTodos)]

newtype FailTodos = FailTodos Bool

instance IsOption FailTodos where
  defaultValue = FailTodos False
  parseValue = fmap FailTodos . safeReadBool
  optionName = pure "fail-todos"
  optionHelp = pure "Make TODO tests fail instead of succeeding"
  optionCLParser = flagCLParser Nothing (FailTodos True)

-- | A TestTree representing a test that will be written at some point.
testTreeTodo :: TestName -> TestTree
testTreeTodo name = SingleTest name TodoTest
