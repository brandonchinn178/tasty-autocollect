{- AUTOCOLLECT.TEST -}

module Test.Tasty.AutoCollect.Utils.TreeMapTest (
-- AUTOCOLLECT.TEST.export

) where

import qualified Data.Map.Strict as Map
import Test.Tasty.HUnit

import Test.Tasty.AutoCollect.Utils.TreeMap

test =
  testCase "builds the correct tree" $
    fromList (zip [["A", "B", "C"], ["A", "B"], ["A", "C", "D"], ["Z"]] [1 :: Int ..])
      @?= TreeMap
        { value = Nothing
        , children =
            Map.fromList
              [ child "A" Nothing $
                  [ child "B" (Just 2) $
                      [ child "C" (Just 1) []
                      ]
                  , child "C" Nothing $
                      [ child "D" (Just 3) []
                      ]
                  ]
              , child "Z" (Just 4) []
              ]
        }

child :: (Ord k) => k -> Maybe v -> [(k, TreeMap k v)] -> (k, TreeMap k v)
child k v sub = (k, TreeMap v (Map.fromList sub))
