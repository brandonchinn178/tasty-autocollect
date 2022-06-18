{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Tasty.AutoCollect.Plugin (plugin) where

import Control.Monad ((>=>))
import GHC.Driver.Main (getHscEnv)
import GHC.Plugins hiding (getHscEnv)
import GHC.Parser.Annotation (
  AnnotationComment (..),
  ApiAnns (..),
 )
import Test.Tasty (TestTree)

import Test.Tasty.AutoCollect.Constants
import Test.Tasty.AutoCollect.ConvertTest
import Test.Tasty.AutoCollect.Error
import Test.Tasty.AutoCollect.GHC

plugin :: Plugin
plugin =
  defaultPlugin
    { dynflagsPlugin = \_ df ->
        pure $ df `gopt_set` Opt_KeepRawTokenStream
    , pluginRecompile = purePlugin
    , parsedResultAction = \_ _ modl -> do
        nameCache <- hsc_NC <$> getHscEnv
        testTreeName <-
          liftIO $
            thNameToGhcNameIO nameCache ''TestTree >>=
              maybe (autocollectError "Could not get Name for TestTree") return

        return $
          case getModuleType modl of
            Just ModuleMain -> transformMainModule modl
            Just ModuleTest -> transformTestModule testTreeName modl
            Nothing -> modl
    }

{----- ModuleType -----}

-- | The type of module being compiled.
data ModuleType = ModuleMain | ModuleTest

getModuleType :: HsParsedModule -> Maybe ModuleType
getModuleType = firstLocatedWhere parseModuleType . apiAnnRogueComments . hpm_annotations

parseModuleType :: RealLocated AnnotationComment -> Maybe ModuleType
parseModuleType = getBlockComment >=> \case
  s
    | isMainComment s -> Just ModuleMain
    | isTestComment s -> Just ModuleTest
    | otherwise -> Nothing

{----- Transform modules -----}

transformMainModule :: HsParsedModule -> HsParsedModule
transformMainModule = trace "transformMainModule"
