{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.Utils.TreeMap (
  TreeMap (..),
  fromList,
  foldTreeMap,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

data TreeMap k v = TreeMap
  { value :: Maybe v
  , children :: Map k (TreeMap k v)
  }
  deriving (Show, Eq)

-- | Convert the given list of values into a 'TreeMap'.
--
-- For example,
-- @
-- fromList [[A, B, C], [A, B], [A, C, D], [Z]]
-- @
-- would become
-- @
-- TreeMap
--   { value = Nothing
--   , children = Map.fromList
--       [ ("A", TreeMap
--           { value = Nothing
--           , children = Map.fromList
--               [ ("B", TreeMap
--                   { value = Just ...
--                   , children = Map.fromList
--                       ("C", [ TreeMap
--                           { value = Just ...
--                           , children = Map.empty
--                           }
--                       ])
--                   })
--               , ("C", TreeMap
--                   { value = Nothing
--                   , children = Map.fromList
--                       [ ("D", TreeMap
--                           { value = Just ...
--                           , children = Map.empty
--                           })
--                       ]
--                   })
--               ]
--           })
--     , ("Z", TreeMap
--         { value = Just ...
--         , children = Map.empty
--         })
--     ]
--   }
-- @
fromList :: (Ord k) => [([k], v)] -> TreeMap k v
fromList = foldr (uncurry insert) empty

empty :: TreeMap k v
empty = TreeMap Nothing Map.empty

insert :: (Ord k) => [k] -> v -> TreeMap k v -> TreeMap k v
insert originalKeys v = go originalKeys
  where
    go ks treeMap =
      case ks of
        [] -> treeMap{value = Just v}
        k : ks' -> treeMap{children = Map.alter (Just . go ks' . fromMaybe empty) k (children treeMap)}

foldTreeMap :: (Maybe v -> Map k r -> r) -> TreeMap k v -> r
foldTreeMap f = go
  where
    go TreeMap{..} = f value (go <$> children)
