{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Core where

import Compiler.Hoopl
import Compiler.Hoopl.Label
import Compiler.Hoopl.Passes.Live
import qualified Data.Char as Char
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty (..))
import qualified Data.Text.Prettyprint.Doc as Pretty
import Numeric.Natural

data Insn :: Type -> Type -> Type where
    Label :: Label -> Insn C O
    BumpStack :: Operand -> Insn O O
    Read :: LogSize -> Operand -> Insn O O
    Write :: LogSize -> Operand -> Operand -> Insn O O
    UnOp :: UnOp -> Operand -> Insn O O
    BinOp :: BinOp -> Operand -> Operand -> Insn O O
    Branch :: BranchCmp -> Operand -> Operand -> Label -> Label -> Insn O C
    UBranch :: Label -> Insn O C
    Jump :: Operand -> [Operand] -> Insn O C
    Unreachable :: Insn O C
deriving instance Eq (Insn i o)
deriving instance Show (Insn i o)

instance NonLocal Insn where
    entryLabel (Label l) = l
    successors = \ case
        Branch _ _ _ l₁ l₂ -> [l₁, l₂]
        UBranch l -> [l]
        _ -> []

instance HooplNode Insn where
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

data Operand = Local Int | Const Const
  deriving (Eq, Show)

data Const = Literal Natural | Global (Either Name Label)
  deriving (Eq, Show)

data BranchCmp = Equal | Less Signedness
  deriving (Eq, Show)

newtype LogSize = LogSize { unLogSize :: Word }
  deriving (Eq, Ord, Show)

newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show)

instance NodeWithVars Insn where
    type Var Insn = Int
    type VarSet Insn = IntSet
    varsUsed = \ case
        Label _ -> mempty
        BumpStack a -> operandVars a
        Read _ a -> operandVars a
        Write _ a d -> foldMap operandVars [a, d]
        UnOp _ a -> operandVars a
        BinOp _ a b -> foldMap operandVars [a, b]
        Branch _ a b _ _ -> foldMap operandVars [a, b]
        UBranch _ -> mempty
        Jump a as -> foldMap operandVars (a:as)
        Unreachable -> mempty
      where
        operandVars = \ case
            Local k -> IS.singleton k
            _ -> mempty
    varsDefd _ = IS.empty
    killsAllVars = \ case
        Unreachable -> True
        _ -> False

instance Pretty (Insn e x) where
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

instance Pretty Operand where
    pretty = \ case
        Local n -> "%" <> pretty (fromIntegral n :: Word)
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
