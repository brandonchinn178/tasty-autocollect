{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Tasty.AutoCollect.ExternalNames (
  ExternalNames (..),
  loadExternalNames,
) where

import Test.Tasty (TestTree)
import Test.Tasty.ExpectedFailure (expectFail, expectFailBecause, ignoreTest, ignoreTestBecause)

import Test.Tasty.AutoCollect.Error
import Test.Tasty.AutoCollect.GHC
import Test.Tasty.Ext.Todo (testTreeTodo)

data ExternalNames = ExternalNames
  { name_String :: Name
  , name_concat :: Name
  , name_map :: Name
  , name_TestTree :: Name
  , name_testTreeTodo :: Name
  , name_expectFail :: Name
  , name_expectFailBecause :: Name
  , name_ignoreTest :: Name
  , name_ignoreTestBecause :: Name
  }

loadExternalNames :: HscEnv -> IO ExternalNames
loadExternalNames env = do
  name_String <- loadName ''String
  name_concat <- loadName 'concat
  name_map <- loadName 'map
  name_TestTree <- loadName ''TestTree
  name_testTreeTodo <- loadName 'testTreeTodo
  name_expectFail <- loadName 'expectFail
  name_expectFailBecause <- loadName 'expectFailBecause
  name_ignoreTest <- loadName 'ignoreTest
  name_ignoreTestBecause <- loadName 'ignoreTestBecause
  pure ExternalNames{..}
  where
    loadName name =
      thNameToGhcNameIO (hsc_NC env) name
        >>= maybe (autocollectError $ "Could not get Name for " ++ show name) pure
