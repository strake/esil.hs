{-# LANGUAGE StrictData #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Asm where

import Compiler.Hoopl (Label)
import Data.Bits
import Data.Char (toLower)
import Data.Int (Int32, Int64)
import Data.Kind
import Data.Text.Prettyprint.Doc (Pretty (..))
import qualified Data.Text.Prettyprint.Doc as Pretty

import Core (LogSize)

data Immediate a = Immediate a | LabelRelValue Label
  deriving (Eq, Show)

data Operand :: Bool -> Type where
    ImmediateOperand :: Immediate Int64 -> Operand False
    RegOperand :: Reg -> Operand rw
    MemOperand :: LogSize -> Addr -> Operand rw
    IPROperand :: LogSize -> Immediate Int32 -> Operand rw

deriving instance Show (Operand rw)

data Addr = Addr
  { baseReg :: Maybe Reg
  , displacement :: Immediate Int32
  , indexReg :: Maybe (Scale, Reg)
  } deriving (Eq, Show)

nullAddr :: Addr
nullAddr = Addr { baseReg = Nothing, displacement = Immediate 0, indexReg = Nothing }

data Scale = Scale1 | Scale2 | Scale4 | Scale8
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Reg = Rax | Rcx | Rdx | Rbx | Rsp | Rbp | Rsi | Rdi | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Line where
    Ret, Nop, PushF, PopF, Cmc, Clc, Stc, Cli, Sti, Cld, Std :: Line
    Inc, Dec, Not, Neg, Bswap :: Operand True -> Line
    Add, Or, Adc, Sbb, And, Sub, Xor, Cmp, Test, Bsf, Bsr, Andn, Imul :: Operand True -> Operand r -> Line
    Rol, Ror, Rcl, Rcr, Shl, Shr, Sar :: Operand True -> Operand r -> Line
    Popcnt, Lzcnt, Tzcnt :: Operand True -> Operand r -> Line
    Xchg :: Operand True -> Operand True -> Line
    Mov :: Maybe Cond -> Operand True -> Operand r -> Line
    Set :: Cond -> Operand True -> Line

    Lea :: Operand True -> Operand True -> Line

    J :: Maybe Cond -> Operand r -> Line

deriving instance Show Line

data Cond = O | NO | B | AE | E | NE | BE | A | S | NS | P | NP | L | GE | LE | G
  deriving (Eq, Ord, Read, Show, Enum, Bounded)
pattern NAE, C, NB, NC, Z, NZ, NA, NBE, PE, PO, NGE, NL, NG, NLE :: Cond
pattern NAE = B
pattern C   = B
pattern NB  = AE
pattern NC  = AE
pattern Z   = E
pattern NZ  = NE
pattern NA  = BE
pattern NBE = A
pattern PE  = P
pattern PO  = NP
pattern NGE = L
pattern NL  = GE
pattern NG  = LE
pattern NLE = G

notCond :: Cond -> Cond
notCond = toEnum . xor 1 . fromEnum

instance Pretty Line where
    pretty = \ case
        i -> pretty . fmap toLower . show $ i
