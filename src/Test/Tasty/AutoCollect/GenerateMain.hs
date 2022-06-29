module Test.Tasty.AutoCollect.GenerateMain (
  generateMainModule,
) where

import Data.Text (Text)

import Test.Tasty.AutoCollect.Config

generateMainModule :: AutoCollectConfig -> FilePath -> IO Text
generateMainModule _ _ = pure ""
