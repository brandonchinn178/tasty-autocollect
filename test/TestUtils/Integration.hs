{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TestUtils.Integration (
  runTest,
  runTestWith,
  runTest_,
  runTestWith_,
  GHCProject (..),
  runghc,
  ExitCode (..),
) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (ExitCode (..), proc, readProcess, setWorkingDir)

-- | Contents of a file broken up by lines.
type FileContents = [Text]

data GHCProject = GHCProject
  { dependencies :: [Text]
  , extraGhcArgs :: [Text]
  , files :: [(FilePath, FileContents)]
  , entrypoint :: FilePath
  }

{- |
Run a test file with tasty-autocollect.

Automatically imports Test.Tasty and Test.Tasty.HUnit.
-}
runTest :: FileContents -> IO (ExitCode, Text, Text)
runTest contents = runTestWith contents id

-- | Same as 'runTest', except allows modifying the project before running.
runTestWith :: FileContents -> (GHCProject -> GHCProject) -> IO (ExitCode, Text, Text)
runTestWith contents f =
  runghc . f $
    GHCProject
      { dependencies = ["tasty", "tasty-hunit"]
      , extraGhcArgs = ["-F", "-pgmF=tasty-autocollect"]
      , files =
          [ ("Test.hs", testFilePrefix ++ contents)
          , ("Main.hs", ["{- AUTOCOLLECT.MAIN -}"])
          ]
      , entrypoint = "Main.hs"
      }
  where
    testFilePrefix =
      [ "{- AUTOCOLLECT.TEST -}"
      , "module Test ({- AUTOCOLLECT.TEST.export -}) where"
      , "import Test.Tasty"
      , "import Test.Tasty.HUnit"
      ]

-- | Same as 'runTest', except throws an error if the run fails.
runTest_ :: FileContents -> IO (Text, Text)
runTest_ contents = runTestWith_ contents id

-- | Same as 'runTestWith', except throws an error if the run fails.
runTestWith_ :: FileContents -> (GHCProject -> GHCProject) -> IO (Text, Text)
runTestWith_ contents f = do
  (code, stdout, stderr) <- runTestWith contents f
  case code of
    ExitSuccess -> return (stdout, stderr)
    ExitFailure _ ->
      errorWithoutStackTrace . unlines $
        [ "Got: " ++ show code
        , "Stdout: " ++ Text.unpack stdout
        , "Stderr: " ++ Text.unpack stderr
        ]

-- | Compile and run the given project.
runghc :: GHCProject -> IO (ExitCode, Text, Text)
runghc GHCProject{..} =
  withSystemTempDirectory "tasty-autocollect-integration-test" $ \tmpdir -> do
    let output = tmpdir </> "test"

    forM_ files $ \(fp, contents) -> Text.writeFile (tmpdir </> fp) (Text.unlines contents)

    let ghcArgs =
          concat
            [ ["-hide-all-packages"]
            , ["-package " <> dep | dep <- dependencies]
            , extraGhcArgs
            ]

    ghcResult@(ghcCode, _, _) <-
      readProcess $
        setWorkingDir tmpdir . proc "ghc" $
          entrypoint : "-o" : output : map Text.unpack ghcArgs

    (runCode, stdout, stderr) <-
      case ghcCode of
        ExitSuccess -> readProcess $ setWorkingDir tmpdir $ proc output []
        ExitFailure _ -> return ghcResult

    let decode = TextL.toStrict . TextL.decodeUtf8
    return (runCode, decode stdout, decode stderr)
