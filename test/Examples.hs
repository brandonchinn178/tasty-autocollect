{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}

module Examples (
  {- AUTOCOLLECT.TEST.export -}
) where

import Data.ByteString.Lazy (ByteString)
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

test_testCase :: Assertion
test_testCase "Addition" = do
  1 + 1 @?= (2 :: Int)
  2 + 2 @?= (4 :: Int)

test_testCase :: Assertion
test_testCase "Reverse" = reverse [1, 2, 3] @?= ([3, 2, 1] :: [Int])

test_testProperty :: [Int] -> Property
test_testProperty "reverse . reverse === id" = \xs -> (reverse . reverse) xs === id xs

test_goldenVsString :: IO ByteString
test_goldenVsString "Example golden test" "example.golden" = pure "example"
