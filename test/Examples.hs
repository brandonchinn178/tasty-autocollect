{- AUTOCOLLECT.TEST -}

module Examples (
  {- AUTOCOLLECT.TEST.export -}
) where

-- import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

testCase :: Assertion
testCase "Addition" = do
  1 + 1 @?= 2
  2 + 2 @?= 4

testCase :: Assertion
testCase "Reverse" = reverse [1, 2, 3] @?= [3, 2, 1]

testProperty :: [Int] -> Property
testProperty "reverse . reverse === id" = \xs -> (reverse . reverse) xs === id xs

-- goldenVsString :: IO ByteString
-- goldenVsString "Some test" "some_test.golden" = do
--   ...
