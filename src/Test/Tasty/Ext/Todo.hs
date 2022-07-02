module Test.Tasty.Ext.Todo (
  testTreeTodo,
) where

import Data.Typeable (Typeable)
import Test.Tasty.Providers (IsTest (..), TestName, testPassed)
import Test.Tasty.Runners (Result (..), TestTree (..))

data TodoTest = TodoTest
  deriving (Typeable)

instance IsTest TodoTest where
  run _ _ _ = pure (testPassed ""){resultShortDescription = "TODO"}
  testOptions = mempty

-- | A TestTree representing a test that will be written at some point.
testTreeTodo :: TestName -> a -> TestTree
testTreeTodo name _ = SingleTest name TodoTest
