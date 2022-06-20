{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.ConvertMain (
  transformMainModule,
) where

import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as Text
import GHC.Hs (
  HsModule (..),
 )
import GHC.Plugins
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, splitExtensions, takeDirectory, (</>))

import Test.Tasty.AutoCollect.Config
import Test.Tasty.AutoCollect.Error
import Test.Tasty.AutoCollect.ExternalNames

{- |
Generates a Main module of the form

@
{-
AUTOCOLLECT.MAIN
suite_name = my-test-suite
<config>
-}
@

to the equivalent of

@
import Test.Tasty (defaultMain)

import qualified MyTest
<find all other test files>

main :: IO ()
main = defaultMain $
  testGroup "my-test-suite" $
    concat
      [ MyTest.tests
      , ...
      ]
@

See 'AutoCollectConfig' for more information on what can be configured.
-}
transformMainModule ::
  AutoCollectConfig ->
  ExternalNames ->
  ModSummary ->
  HsParsedModule ->
  IO HsParsedModule
transformMainModule cfg names ms parsedModl = do
  mainFilePath <-
    maybe (autocollectError "Could not load location of Main module") return $
      ml_hs_file (ms_location ms)

  testModules <- findTestModules mainFilePath

  let suiteName = fromMaybe mainFilePath $ cfgSuiteName cfg
  mainModl <- mkMainModule cfg suiteName names testModules
  pure parsedModl{hpm_module = mainModl <$ hpm_module parsedModl}

mkMainModule :: AutoCollectConfig -> String -> ExternalNames -> [String] -> IO HsModule
mkMainModule _ _ _ _ = undefined

findTestModules :: FilePath -> IO [String]
findTestModules mainFilePath = mapMaybe toModule . filter (/= mainFilePath) <$> listDirectoryRecursive testDir
  where
    testDir = takeDirectory mainFilePath
    toModule fp =
      case splitExtensions (makeRelative testDir fp) of
        (name, ".hs") -> Just $ Text.unpack . Text.replace "/" "." . Text.pack $ name
        _ -> Nothing

{----- File system -----}

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fp = fmap concat . mapM (go . (fp </>)) =<< listDirectory fp
  where
    go child = do
      isDir <- doesDirectoryExist child
      if isDir
        then listDirectoryRecursive child
        else pure [child]
