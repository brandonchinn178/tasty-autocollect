module Test.Tasty.AutoCollect.Utils.Tree (
  Tree (..),
  toTree,
) where

import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq

data Tree k v = Tree
  { fullPath :: [k]
  , value :: Maybe v
  , subTrees :: [Tree k v]
  }
  deriving (Show, Eq)

data TreeMap k v = TreeMap
  { value' :: Maybe v
  , children' :: Map k (TreeMap k v)
  }

{- |
Convert the given list of values into a tree.

For example,
@
toTree [[A, B, C], [A, B], [A, C, D], [Z]]
@
would become
@
Tree
  { fullPath = []
  , value = Nothing
  , subTrees =
      [ Tree
        { fullPath = [A]
        , value = Nothing
        , subTrees =
            [ Tree
                { fullPath = [A, B]
                , value = Just ...
                , subTrees =
                    [ Tree
                        { fullPath = [A, B, C]
                        , value = Just ...
                        , subTrees = []
                        }
                    ]
                }
            , Tree
                { fullPath = [A, C]
                , value = Nothing
                , subTrees =
                    [ Tree
                        { fullPath = [A, C, D]
                        , value = Just ...
                        , subTrees = []
                        }
                    ]
                }
            ]
        }
    , Tree
        { fullPath = [Z]
        , value = Just ...
        , subTrees = []
        }
    ]
  }
@
-}
toTree :: Ord k => [([k], v)] -> Tree k v
toTree = fromTreeMap Seq.empty . fromList
  where
    fromTreeMap path (TreeMap mVal children) =
      Tree
        { fullPath = toList path
        , value = mVal
        , subTrees = Map.elems $ Map.mapWithKey (fromTreeMap . (path |>)) children
        }

{- |
Convert the given list of values into a 'TreeMap'.

For example,
@
fromList [[A, B, C], [A, B], [A, C, D], [Z]]
@
would become
@
TreeMap
  { value = Nothing
  , children = Map.fromList
      [ ("A", TreeMap
          { value = Nothing
          , children = Map.fromList
              [ ("B", TreeMap
                  { value = Just ...
                  , children = Map.fromList
                      ("C", [ TreeMap
                          { value = Just ...
                          , children = Map.empty
                          }
                      ])
                  })
              , ("C", TreeMap
                  { value = Nothing
                  , children = Map.fromList
                      [ ("D", TreeMap
                          { value = Just ...
                          , children = Map.empty
                          })
                      ]
                  })
              ]
          })
    , ("Z", TreeMap
        { value = Just ...
        , children = Map.empty
        })
    ]
  }
@
-}
fromList :: Ord k => [([k], v)] -> TreeMap k v
fromList = foldr (uncurry insert) empty

empty :: TreeMap k v
empty = TreeMap Nothing Map.empty

insert :: Ord k => [k] -> v -> TreeMap k v -> TreeMap k v
insert originalKeys v = go originalKeys
  where
    go ks treeMap =
      case ks of
        [] -> treeMap{value' = Just v}
        k : ks' -> treeMap{children' = Map.alter (Just . go ks' . fromMaybe empty) k (children' treeMap)}
