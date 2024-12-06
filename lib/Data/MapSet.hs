{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Data.MapSet where

import Data.Map   (Map)
import Data.Map   qualified as M
import Data.Set   (Set)
import GHC.IsList

newtype MapSet m s = MapSet {
    getMapSet :: Map m (Set s)
} deriving (Eq, Show)

instance (Ord m) ⇒ IsList (MapSet m s) where
    type Item (MapSet m s) = (m, Set s)
    fromList = MapSet . fromList
    toList = toList . getMapSet

instance (Ord m, Ord s) ⇒ Semigroup (MapSet m s) where
    MapSet x <> MapSet y = MapSet (M.unionWith (<>) x y)

instance (Ord m, Ord s) ⇒ Monoid (MapSet m s) where
    mempty = []
