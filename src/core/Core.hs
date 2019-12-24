{-# LANGUAGE StrictData #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Core where

import Prelude hiding (Functor, (<$>), Monad)
import qualified Prelude as Base
import Compiler.Hoopl hiding ((<*>))
import Compiler.Hoopl.Label
import Compiler.Hoopl.Passes.Live
import Control.Categorical.Functor
import Control.Categorical.Monad
import qualified Data.Char as Char
import Data.Functor.Identity (Identity (..))
import Data.GenericTrie (Trie, TrieKey)
import Data.Kind (Type)
import qualified Data.Set.Class as Set
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty (..))
import qualified Data.Text.Prettyprint.Doc as Pretty
import Numeric.Natural

import Data.MapSet
import Orphans ()

data Insn v :: Type -> Type -> Type where
    Label :: Label -> Insn v C O
    BumpStack :: Operand v -> Insn v O O
    Read :: LogSize -> Operand v -> Insn v O O
    Write :: LogSize -> Operand v -> Operand v -> Insn v O O
    UnOp :: UnOp -> Operand v -> Insn v O O
    BinOp :: BinOp -> Operand v -> Operand v -> Insn v O O
    Branch :: BranchCmp -> Operand v -> Operand v -> Label -> Label -> Insn v O C
    UBranch :: Label -> Insn v O C
    Jump :: Operand v -> [Operand v] -> Insn v O C
    Unreachable :: Insn v O C
deriving instance Eq v => Eq (Insn v i o)
deriving instance Show v => Show (Insn v i o)

traverseInsn :: Applicative p => (u -> p v) -> Insn u i o -> p (Insn v i o)
traverseInsn f = \ case
    Label l -> pure (Label l)
    BumpStack a -> BumpStack <$> traverse f a
    Read lsz a -> Read lsz <$> traverse f a
    Write lsz a d -> Write lsz <$> traverse f a <*> traverse f d
    UnOp op a -> UnOp op <$> traverse f a
    BinOp op a b -> BinOp op <$> traverse f a <*> traverse f b
    Branch br a b i j -> (\ f -> f i j) <$> (Branch br <$> traverse f a <*> traverse f b)
    UBranch l -> pure (UBranch l)
    Jump a as -> Jump <$> traverse f a <*> (traverse . traverse) f as
    Unreachable -> pure Unreachable

instance (Applicative m, Monad (->) m) => Functor (Kleisli (->) m) (NT (NT (Kleisli (->) m))) Insn where
    map (Kleisli f) = NT (NT (Kleisli (traverseInsn f)))

instance Functor (->) (NT (NT (->))) Insn where
    map f = NT (NT (runIdentity . traverseInsn (Identity . f)))

instance NonLocal (Insn v) where
    entryLabel (Label l) = l
    successors = \ case
        Branch _ _ _ l₁ l₂ -> [l₁, l₂]
        UBranch l -> [l]
        _ -> []

instance HooplNode (Insn v) where
    mkBranchNode = UBranch
    mkLabelNode = Label

data UnOp
  = Ham | Clz | Ctz
  deriving (Eq, Show)

data BinOp
  = Add | Sub | And | Or | Xor | Andc | Orc | Xnor | Nand | Nor
  | Grev | Gorc | Shfl
  | Shift (Shift Signedness) | Rotate (Shift ())
  | Min Signedness | Max Signedness
  | Slt Signedness
  | Mul | MulH Signedness'
  | XMul | XMulH
  | Div Signedness | Rem Signedness
  deriving (Eq, Show)

type Signedness = Bool

data Signedness' = UU | SU | SS
  deriving (Eq, Show)

data Shift s = ShiftL | ShiftR s
  deriving (Eq, Show)

data Operand v = Local v | Const Const
  deriving (Eq, Show, Foldable, Base.Functor, Traversable)

data Const = Literal Natural | Global (Either Name Label)
  deriving (Eq, Show)

data BranchCmp = Equal | Less Signedness
  deriving (Eq, Show)

newtype LogSize = LogSize { unLogSize :: Word }
  deriving (Eq, Ord, Show)

newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show)

instance TrieKey k => NodeWithVars (Insn k) where
    type VarSet (Insn k) = MapSet (Trie k)
    varsUsed = \ case
        Label _ -> Set.empty
        BumpStack a -> operandVars a
        Read _ a -> operandVars a
        Write _ a d -> foldMap operandVars [a, d]
        UnOp _ a -> operandVars a
        BinOp _ a b -> foldMap operandVars [a, b]
        Branch _ a b _ _ -> foldMap operandVars [a, b]
        UBranch _ -> Set.empty
        Jump a as -> foldMap operandVars (a:as)
        Unreachable -> Set.empty
      where
        operandVars :: TrieKey v => Operand v -> MapSet (Trie v)
        operandVars = foldMap Set.singleton
    varsDefd _ = Set.empty
    killsAllVars = \ case
        Unreachable -> True
        _ -> False

instance Pretty v => Pretty (Insn v e x) where
    pretty = \ case
        Label (Lbl l) -> pretty l <> ":"
        BumpStack n -> Pretty.hsep ["bump-stack", pretty n]
        Read (LogSize w) a -> Pretty.hsep ["read" , pretty w, pretty a]
        Write (LogSize w) a d -> Pretty.hsep ["write", pretty w, pretty a, pretty d]
        UnOp op a -> Pretty.hsep [pretty op, pretty a]
        BinOp op a b -> Pretty.hsep [pretty op, pretty a, pretty b]
        Branch br a b (Lbl i) (Lbl j) -> Pretty.hsep ["branch", pretty br, pretty a, pretty b, pretty i, pretty j]
        UBranch (Lbl l) -> "branch" Pretty.<+> pretty l
        Jump a as -> Pretty.hsep ("jump" : pretty a : fmap pretty as)
        Unreachable -> "unreachable"

instance Pretty v => Pretty (Operand v) where
    pretty = \ case
        Local v -> "%" <> pretty v
        Const c -> pretty c

instance Pretty Const where
    pretty = \ case
        Literal n -> pretty n
        Global g -> "@" <> case g of
            Left name -> pretty name
            Right (Lbl l) -> pretty l

instance Pretty UnOp where pretty = pretty . fmap Char.toLower . show
instance Pretty BinOp where pretty = pretty . fmap Char.toLower . show
instance Pretty BranchCmp where pretty = pretty . fmap Char.toLower . show

instance Pretty Name where pretty = pretty . unName

pattern Lbl :: Int -> Label
pattern Lbl l <- (lblToUnique -> l)
  where Lbl l = uniqueToLbl l
{-# COMPLETE Lbl #-}
