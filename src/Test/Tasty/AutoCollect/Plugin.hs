{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.Plugin (plugin) where

import GHC.Driver.Main (getHscEnv)
import GHC.Plugins hiding (getHscEnv)
import GHC.Parser.Annotation (
  AnnotationComment (..),
  ApiAnns (..),
 )

import Test.Tasty.AutoCollect.Config
import Test.Tasty.AutoCollect.Constants
import Test.Tasty.AutoCollect.ConvertMain
import Test.Tasty.AutoCollect.ConvertTest
import Test.Tasty.AutoCollect.Error
import Test.Tasty.AutoCollect.ExternalNames
import Test.Tasty.AutoCollect.GHC

plugin :: Plugin
plugin =
  defaultPlugin
    { dynflagsPlugin = \_ df ->
        pure $ df `gopt_set` Opt_KeepRawTokenStream
    , pluginRecompile = purePlugin
    , parsedResultAction = \_ ms modl -> do
        env <- getHscEnv
        names <- liftIO $ loadExternalNames env

        liftIO $
          case getModuleType modl of
            Just (ModuleMain cfg) -> transformMainModule cfg names ms modl
            Just ModuleTest -> transformTestModule names modl
            Nothing -> pure modl
    }

{----- ModuleType -----}

-- | The type of module being compiled.
data ModuleType
  = ModuleMain AutoCollectConfig
  | ModuleTest

getModuleType :: HsParsedModule -> Maybe ModuleType
getModuleType = firstLocatedWhere parseModuleType . apiAnnRogueComments . hpm_annotations

parseModuleType :: RealLocated AnnotationComment -> Maybe ModuleType
parseModuleType loc = do
  comment <- getBlockComment loc
  let (firstLine, remainingComment) = break (== '\n') comment
  if
      | isMainComment firstLine -> do
          config <- either autocollectError pure $ parseConfig remainingComment
          pure $ ModuleMain config
      | isTestComment comment ->
          pure ModuleTest
      | otherwise ->
          Nothing
