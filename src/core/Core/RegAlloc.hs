{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Core.RegAlloc where

import Prelude hiding (Functor, (<$>), Monad, map)
import Compiler.Hoopl hiding ((<*>))
import Compiler.Hoopl.Passes.Live
import Control.Applicative (Alternative (..))
import Control.Categorical.Functor
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.State (evalState)
import qualified Control.Monad.Trans.State as M
import Data.Coerce
import Data.GenericTrie (Trie, TrieKey)
import Data.GenericTrie.Internal (Trie (..))
import Data.IntSet (IntSet)
import qualified Data.IntMap as IM
import Data.Map.Class as Map
import qualified RegAlloc.Hoopl as Hoopl
import Util

import Core
import Data.Assignment
import Data.MapSet
import Orphans ()

allocRegs :: (TrieKey v, LabelsPtr entry) => RegCount -> MaybeC i entry -> Graph (Assigned v (Insn v)) i C -> Except _ (Graph (Assigned (v, Int) (Insn (v, Int))) i C)
allocRegs c entry = map (mapGraph unInsn') . Hoopl.allocRegs vars isMove c entry . mapGraph Insn' . renumerateGraph
  where
    vars :: (Applicative p) => (Int -> p Int) -> Insn' v i o -> p (Insn' v i o)
    vars = \ f (Insn' insn') -> Insn' <$> bitraverseAssigned (traverse f) (traverse f) insn'

    isMove :: Insn' v i o -> Maybe Hoopl.MoveSpec
    isMove = unInsn' & rhs & \ insn -> do
        BinOp op a b <- pure insn
        let f a b = [Hoopl.MoveSpec (Hoopl.Node v)
                    | Local (_, v) <- pure a, Core.Const (Literal n) <- pure b, isNeutral op n]
        f a b <|> f b a

    isNeutral op n = op ∈ [Add, Sub, Or, Xor] && n == 0

renumerateGraph :: TrieKey v => Graph (Assigned v (Insn v)) i o -> Graph (Assigned (v, Int) (Insn (v, Int))) i o
renumerateGraph = flip evalState (Map.empty :: Trie _ _) . bitraverseGraphBinders (φ lookupM) (φ lookupM)
  where
    lookupM v = M.get >>= \ ks -> case ks !? v of
        Just k -> pure k
        Nothing -> let k = length ks in k <$ M.modify (insert v k)
    φ f = fmap <$> (,) <*> f

type RegCount = Int

newtype Insn' v i o = Insn' { unInsn' :: Assigned (v, Int) (Insn (v, Int)) i o }
  deriving (NonLocal, HooplNode)

instance NodeWithVars (Insn' v) where
    type VarSet (Insn' v) = IntSet
    killsAllVars = killsAllVars . bimapAssigned snd snd . unInsn'
    varsUsed = coerce (IM.keysSet :: _ () -> _) . varsUsed . bimapAssigned snd snd . unInsn'
    varsDefd = coerce (IM.keysSet :: _ () -> _) . varsDefd . bimapAssigned snd snd . unInsn'
