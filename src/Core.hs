{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Core where

import Compiler.Hoopl
import Compiler.Hoopl.Passes.Live
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Kind (Type)
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

data BinOp
  = Add | Sub | And | Or | Xor | Andc | Orc | Xnor | Nand | Nor
  | Grev | Gorc | Shfl
  | Shift (Shift Signedness) | Rotate (Shift ())
  | Min Signedness | Max Signedness
  | Slt Signedness
  | Mul | MulH Signedness'
  | XMul | XMulH
  | Div Signedness | Rem Signedness

type Signedness = Bool

data Signedness' = UU | SU | SS

data Shift s = ShiftL | ShiftR s

data Operand = Local Int | Const Const

data Const = Literal Natural | Global Name

data BranchCmp = Equal | Less Signedness

newtype LogSize = LogSize { unLogSize :: Word }
  deriving (Eq, Ord, Show)

newtype Name = Name { unName :: [Char] }
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
