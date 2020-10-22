{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Core.RegAlloc where

import Prelude hiding (Functor, (<$>), Monad, map)
import Compiler.Flow.Graph (Graph)
import qualified Compiler.Flow.Graph as Graph
import Compiler.Flow.NonLocal as Flow.NonLocal
import Compiler.Flow.Pass.Live
import Compiler.Flow.Shape
import Control.Applicative (Alternative (..))
import Control.Categorical.Functor
import Control.Lens (LensLike', coerced)
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.State (evalState)
import qualified Control.Monad.Trans.State as M
import Data.Coerce
import Data.GenericTrie (Trie, TrieKey)
import Data.GenericTrie.Internal (Trie (..))
import Data.IntSet (IntSet)
import qualified Data.IntMap as IM
import Data.Map.Class as Map
import Data.Map.Fresh.Class (FreshMap)
import qualified RegAlloc.Flow
import qualified RegAlloc.Flow as Flow (allocRegs)
import Util

import Core
import Data.Assignment
import Data.MapSet
import Orphans ()

allocRegs
 :: (TrieKey v, FreshMap map, Flow.NonLocal.Label (Insn' v) ~ Key map)
 => RegCount -> MaybeC i (map ()) -> Graph map (Assigned v (Insn v)) i C
 -> Except _ (Graph map (Assigned (v, Int) (Insn (v, Int))) i C)
allocRegs c entry = map (Graph.map' unInsn') . Flow.allocRegs vars isMove [1..c] entry . Graph.map' Insn' . renumerateGraph
  where
    vars :: (Applicative p) => (Int -> p Int) -> Insn' v i o -> p (Insn' v i o)
    vars = \ f (Insn' insn') -> Insn' <$> bitraverseAssigned (traverse f) (traverse f) insn'

    isMove :: Insn' v i o -> Maybe (RegAlloc.Flow.MoveSpec (Either a Int))
    isMove = unInsn' & rhs & \ insn -> do
        BinOp op a b <- pure insn
        let f a b = [RegAlloc.Flow.MoveSpec (Right v)
                    | Local (_, v) <- pure a, Core.Const (Literal n) <- pure b, isNeutral op n]
        f a b <|> f b a

    isNeutral op n = op ∈ [Add, Sub, Or, Xor] && n == 0

renumerateGraph
 :: (TrieKey v, Traversable map)
 => Graph map (Assigned v (Insn v)) i o -> Graph map (Assigned (v, Int) (Insn (v, Int))) i o
renumerateGraph = flip evalState (Map.empty :: Trie _ _) . bitraverseGraphBinders (φ lookupM) (φ lookupM)
  where
    lookupM v = M.get >>= \ ks -> case ks !? v of
        Just k -> pure k
        Nothing -> let k = length ks in k <$ M.modify (insert v k)
    φ f = fmap <$> (,) <*> f

type RegCount = Int

newtype Insn' v i o = Insn' { unInsn' :: Assigned (v, Int) (Insn (v, Int)) i o }
  deriving (Eq, Show)

instance NonLocal (Insn' v) where
    type Label (Insn' v) = Core.Label
    entryLabelL = (coerced :: LensLike' _ (Insn' v C _j) (Assigned (v, Int) (Insn (v, Int)) C _j)) . rhsL . entryLabelL
    successorsL = (coerced :: LensLike' _ (Insn' v _i C) (Assigned (v, Int) (Insn (v, Int)) _i C)) . rhsL . successorsL

instance HasVars (Insn' v) where
    type VarSet (Insn' v) = IntSet
    killsAllVars = killsAllVars . bimapAssigned snd snd . unInsn'
    varsUsed = coerce (IM.keysSet :: _ () -> _) . varsUsed . bimapAssigned snd snd . unInsn'
    varsDefd = coerce (IM.keysSet :: _ () -> _) . varsDefd . bimapAssigned snd snd . unInsn'
