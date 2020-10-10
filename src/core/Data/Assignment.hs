{-# LANGUAGE DataKinds #-}

module Data.Assignment where

import Compiler.Flow.NonLocal
import Compiler.Flow.Pass.Live
import Compiler.Flow.Shape (End (..))
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
    type Label (Assigned k n) = Label n
    entryLabel = entryLabel . rhs
    successors = successors . rhs

instance (HasVars n, Set.Elem (VarSet n) ~ k) => HasVars (Assigned k n) where
    type VarSet (Assigned k n) = VarSet n
    varsUsed = varsUsed . rhs
    varsDefd (Assigned ks n) = foldr Set.insert (varsDefd n) ks
    killsAllVars = killsAllVars . rhs
