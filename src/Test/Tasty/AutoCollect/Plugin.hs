{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Tasty.AutoCollect.Plugin (plugin) where

import Control.Monad ((>=>))
import Data.Char (toLower)
import GHC.Driver.Main (getHscEnv)
import GHC.Plugins hiding (getHscEnv)
import GHC.Parser.Annotation (
  AnnotationComment (..),
  ApiAnns (..),
 )
import Test.Tasty (TestTree)

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

{----- AutoCollectAnn -----}

-- | A tasty-autocollect annotation.
data AutoCollectAnn
  = AutoCollectMain
  | AutoCollectTest
  | AutoCollectTestExport

parseAutoCollectAnn :: RealLocated AnnotationComment -> Maybe AutoCollectAnn
parseAutoCollectAnn = \case
  L _ (AnnBlockComment s) ->
    case map toLower s of
      "{- autocollect.main -}" -> Just AutoCollectMain
      "{- autocollect.test -}" -> Just AutoCollectTest
      "{- autocollect.test.export -}" -> Just AutoCollectTestExport
      _ -> Nothing
  _ -> Nothing

{----- ModuleType -----}

-- | The type of module being compiled.
data ModuleType = ModuleMain | ModuleTest

getModuleType :: HsParsedModule -> Maybe ModuleType
getModuleType = firstLocatedWhere parseModuleType . apiAnnRogueComments . hpm_annotations

parseModuleType :: RealLocated AnnotationComment -> Maybe ModuleType
parseModuleType = parseAutoCollectAnn >=> \case
  AutoCollectMain -> Just ModuleMain
  AutoCollectTest -> Just ModuleTest
  _ -> Nothing

{----- Transform modules -----}

transformMainModule :: HsParsedModule -> HsParsedModule
transformMainModule = trace "transformMainModule"
