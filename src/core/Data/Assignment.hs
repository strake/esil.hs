module Data.Assignment where

import Compiler.Hoopl
import Compiler.Hoopl.Passes.Live
import Control.Applicative
import qualified Data.Set.Class as Set

data Assigned k n i o = Assigned { lhs :: Assignment (Const ()) i o k, rhs :: n i o }

data Assignment argu i o k where
    Argu :: argu k -> Assignment argu C O k
    Lhs :: Maybe k -> Assignment argu O O k
    NoLhs :: Assignment argu O C k
deriving instance Foldable argu => Foldable (Assignment argu i o)
deriving instance Functor argu => Functor (Assignment argu i o)
deriving instance Traversable argu => Traversable (Assignment argu i o)

instance NonLocal n => NonLocal (Assigned k n) where
    entryLabel = entryLabel . rhs
    successors = successors . rhs

instance (HooplNode n) => HooplNode (Assigned k n) where
    mkBranchNode = Assigned NoLhs . mkBranchNode
    mkLabelNode = Assigned (Argu (Const ())) . mkLabelNode

instance (NodeWithVars n, Var n ~ k) => NodeWithVars (Assigned k n) where
    type VarSet (Assigned k n) = VarSet n
    varsUsed = varsUsed . rhs
    varsDefd (Assigned ks n) = foldr Set.insert (varsDefd n) ks
    killsAllVars = killsAllVars . rhs
