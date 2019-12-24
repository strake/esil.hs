{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans () where

import Data.Either.Both
import Data.Filtrable
import qualified Data.GenericTrie as Trie
import Data.GenericTrie.Internal
import Data.Map.Class
import Data.Text as Text
import Util

instance TrieKey k => Filtrable (Trie k) where
    mapMaybe = Trie.mapMaybe

instance TrieKey k => StaticMap (Trie k) where
    type Key (Trie k) = k
    adjustA = flip Trie.at . traverse
    traverseWithKey = Trie.traverseWithKey

instance TrieKey k => Map (Trie k) where
    empty = Trie.empty
    alterF = flip Trie.at
    mergeA f = mapMaybeA id ∘∘ trieMergeWithKey (\ k a b -> pure $ f k (Both a b)) (Trie.mapMaybeWithKey $ \ k a -> Just $ f k (JustLeft a)) (Trie.mapMaybeWithKey $ \ k b -> Just $ f k (JustRight b))
    mapMaybeWithKeyA = Trie.traverseMaybeWithKey

instance TrieKey Text where
    type TrieRep Text = Trie [Char]
    trieEmpty = MkTrie trieEmpty
    trieNull = trieNull . trieRep
    trieLookup = \ k -> trieLookup (Text.unpack k) . trieRep
    trieInsert = \ k a -> MkTrie . trieInsert (Text.unpack k) a . trieRep
    trieDelete = \ k -> MkTrie . trieDelete (Text.unpack k) . trieRep
    trieSingleton = \ k -> MkTrie . trieSingleton (Text.unpack k)
    trieMap = \ f -> MkTrie . trieMap f . trieRep
    trieTraverse = trieTraverseWithKey . pure
    trieFoldWithKey = \ f -> (. trieRep) . trieFoldWithKey (f . Text.pack)
    trieMapMaybeWithKey = \ f -> MkTrie . trieMapMaybeWithKey (f . Text.pack) . trieRep
    trieTraverseWithKey = \ f -> fmap MkTrie . trieTraverseWithKey (f . Text.pack) . trieRep
    trieTraverseMaybeWithKey = \ f -> fmap MkTrie . trieTraverseMaybeWithKey (f . Text.pack) . trieRep
    trieMergeWithKey = \ f a b -> MkTrie ∘∘ compose2 (trieMergeWithKey (f . Text.pack) (trieRep . a . MkTrie) (trieRep . b . MkTrie)) trieRep trieRep

trieRep :: Trie k a -> TrieRep k a
trieRep (MkTrie trie) = trie
