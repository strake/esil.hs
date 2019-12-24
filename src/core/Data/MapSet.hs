{-# LANGUAGE DerivingVia #-}
module Data.MapSet where

import Data.Foldable
import Data.Function (on)
import Data.Map.Class as Map
import Data.Set.Class (Set)
import qualified Data.Set.Class as Set
import Data.Maybe (isJust)
import Util

newtype MapSet map = MapSet { unMapSet :: map () }
  deriving (Semigroup, Monoid) via Union map ()

instance Map map => Set (MapSet map) where
    type Elem (MapSet map) = Key map
    null = null . unMapSet
    size = length . unMapSet
    member k = isJust . (!? k) . unMapSet
    empty = MapSet Map.empty
    singleton = MapSet . flip singleton ()
    insert k = MapSet . insert k () . unMapSet
    delete k = MapSet . delete k    . unMapSet
    union = MapSet ∘∘ unUnion ∘∘ (<>) `on` Union . unMapSet
    intersection = MapSet ∘∘ unIntersection ∘∘ (<>) `on` Intersection . unMapSet
    difference = MapSet ∘∘ difference `on` unMapSet
    (⊆) = null ∘∘ unMapSet ∘∘ Set.difference
    foldr f = (. unMapSet) . foldrWithKey (\ k () -> f k)
    toList = toList . keys . unMapSet
    fromList = MapSet . fromList . fmap (flip (,) ())
