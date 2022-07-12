{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}

module Examples (
  -- $AUTOCOLLECT.TEST.export$
) where

import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

test = testCase "Addition" $ do
  1 + 1 @?= (2 :: Int)
  2 + 2 @?= (4 :: Int)

test = testCase "Reverse" $ reverse [1, 2, 3] @?= ([3, 2, 1] :: [Int])

test = testProperty "reverse . reverse === id" $ \xs -> (reverse . reverse) xs === id (xs :: [Int])

test_prop :: [Int] -> Property
test_prop "reverse . reverse === id" xs = (reverse . reverse) xs === id xs

test = goldenVsString "Example golden test" "test/golden/example.golden" $ pure "example"
