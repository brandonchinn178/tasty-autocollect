{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Tasty.AutoCollect.ExternalNames (
  ExternalNames (..),
  loadExternalNames,
) where

import Test.Tasty (TestTree)

import Test.Tasty.AutoCollect.Error
import Test.Tasty.AutoCollect.GHC
import Test.Tasty.Ext.Todo (testTreeTodo)

data ExternalNames = ExternalNames
  { name_TestTree :: Name
  , name_testTreeTodo :: Name
  , name_concat :: Name
  }

loadExternalNames :: HscEnv -> IO ExternalNames
loadExternalNames env = do
  name_TestTree <- loadName ''TestTree
  name_testTreeTodo <- loadName 'testTreeTodo
  name_concat <- loadName 'concat
  pure ExternalNames{..}
  where
    loadName name =
      thNameToGhcNameIO env (hsc_NC env) name
        >>= maybe (autocollectError $ "Could not get Name for " ++ show name) return
