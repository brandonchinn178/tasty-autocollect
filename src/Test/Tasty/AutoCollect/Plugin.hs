{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.Plugin (plugin) where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Plugins
import GHC.Parser.Annotation (AnnotationComment (..), ApiAnns (..))

plugin :: Plugin
plugin =
  defaultPlugin
    { dynflagsPlugin = \_ df ->
        pure $ df `gopt_set` Opt_KeepRawTokenStream
    , parsedResultAction = \_ _ modl -> do
        return $
          case getModuleType modl of
            Just ModuleMain -> transformMainModule modl
            Just ModuleTest -> transformTestModule modl
            Nothing -> modl
    }

-- | The type of module being compiled.
data ModuleType = ModuleMain | ModuleTest

getModuleType :: HsParsedModule -> Maybe ModuleType
getModuleType =
  listToMaybe
  . mapMaybe parseModuleType
  . sortOn getRealSrcSpan -- ensure we find the first matching comment
  . (apiAnnRogueComments . hpm_annotations)

parseModuleType :: RealLocated AnnotationComment -> Maybe ModuleType
parseModuleType = \case
  L _ (AnnBlockComment s) ->
    case map toLower s of
      "{- autocollect.main -}" -> Just ModuleMain
      "{- autocollect.test -}" -> Just ModuleTest
      _ -> Nothing
  _ -> Nothing

transformMainModule :: HsParsedModule -> HsParsedModule
transformMainModule = trace "transformMainModule"

transformTestModule :: HsParsedModule -> HsParsedModule
transformTestModule = trace "transformTestModule"
