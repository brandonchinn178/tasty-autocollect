{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Tasty.AutoCollect.ExternalNames (
  ExternalNames (..),
  loadExternalNames,
) where

import GHC.Plugins
import Test.Tasty (TestTree)

import Test.Tasty.AutoCollect.Error
import Test.Tasty.AutoCollect.GHC

data ExternalNames = ExternalNames
  { name_TestTree :: Name
  }

loadExternalNames :: HscEnv -> IO ExternalNames
loadExternalNames env = do
  name_TestTree <- loadName ''TestTree
  pure ExternalNames{..}
  where
    loadName name =
      thNameToGhcNameIO (hsc_NC env) name
        >>= maybe (autocollectError $ "Could not get Name for " ++ show name) return
