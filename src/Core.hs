{-# LANGUAGE StrictData #-}
module Core where

import Numeric.Natural

newtype Name = Name { unName :: Word }
  deriving (Eq, Ord, Show)

data Instruction
  = Φ [(Operand, Name)]
  | BumpStack Operand
  | Read LogSize Operand
  | Write LogSize Operand Operand
  | UnOp UnOp Operand
  | BinOp BinOp Operand Operand

data Terminator
  = Branch BranchCmp Operand Operand Name Name
  | Jump Operand [Operand]
  | Unreachable

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

data Operand = Local Name | Const Const

data Const = Literal Natural | Global Name

data BranchCmp = Equal | Less Signedness

newtype LogSize = LogSize { unLogSize :: Word }
  deriving (Eq, Ord, Show)
