{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Assignment where

import Compiler.Flow.NonLocal
import Compiler.Flow.Pass.Live
import Compiler.Flow.Shape (End (..))
import Control.Applicative
import Control.Lens (set)
import Control.Lens.TH (lensField, lensRules, makeLensesWith, mappingNamer)
import qualified Data.Set.Class as Set

data Assigned k n i o = Assigned { lhs :: Assignment (Const ()) i o k, rhs :: n i o }
  deriving (Eq, Show)

data Assignment argu i o k where
    Argu :: argu k -> Assignment argu C O k
    Lhs :: Maybe k -> Assignment argu O O k
    NoLhs :: Assignment argu O C k
deriving instance (Eq k, Eq (argu k)) => Eq (Assignment argu i o k)
deriving instance (Show k, Show (argu k)) => Show (Assignment argu i o k)
deriving instance Foldable argu => Foldable (Assignment argu i o)
deriving instance Functor argu => Functor (Assignment argu i o)
deriving instance Traversable argu => Traversable (Assignment argu i o)

makeLensesWith (set lensField (mappingNamer $ pure . (<|> "L")) lensRules) ''Assigned

instance NonLocal n => NonLocal (Assigned k n) where
    type Label (Assigned k n) = Label n
    entryLabelL = rhsL . entryLabelL
    successorsL = rhsL . successorsL

instance (HasVars n, Set.Elem (VarSet n) ~ k) => HasVars (Assigned k n) where
    type VarSet (Assigned k n) = VarSet n
    varsUsed = varsUsed . rhs
    varsDefd (Assigned ks n) = foldr Set.insert (varsDefd n) ks
    killsAllVars = killsAllVars . rhs
