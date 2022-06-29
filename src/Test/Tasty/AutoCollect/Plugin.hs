{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.Plugin (plugin) where

import GHC.Driver.Main (getHscEnv)
import GHC.Plugins hiding (getHscEnv)

import Test.Tasty.AutoCollect.ConvertTest
import Test.Tasty.AutoCollect.ExternalNames

-- | The plugin to convert a test file. See Preprocessor.hs
plugin :: Plugin
plugin =
  defaultPlugin
    { dynflagsPlugin = \_ df ->
        -- TODO: for some reason, without Opt_KeepRawTokenStream, we get
        -- unused-top-binds errors for generated exports. Ticket?
        pure $ df `gopt_set` Opt_KeepRawTokenStream
    , pluginRecompile = purePlugin
    , parsedResultAction = \_ _ modl -> do
        env <- getHscEnv
        names <- liftIO $ loadExternalNames env
        pure $ transformTestModule names modl
    }
