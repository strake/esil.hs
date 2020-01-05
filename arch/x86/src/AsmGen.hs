{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module AsmGen where

import Compiler.Hoopl hiding ((<*>))
import Control.Applicative
import Control.Monad (join)
import Data.Bool (bool)
import Data.Foldable (toList)
import qualified Data.Map.Class as Map
import qualified Data.Text as Text
import Util
import Util.List

import Core hiding (Const)
import qualified Core
import Data.Assignment

import Asm (Reg)
import qualified Asm

doGraphOC :: Graph (Assigned Reg (Insn Reg)) O C -> ([Asm.Line], LabelMap [Asm.Line])
doGraphOC = \ case
    GMany (JustO entry) body NothingO -> (doBlockOX entry, doBody body)

doBody :: Body (Assigned Reg (Insn Reg)) -> LabelMap [Asm.Line]
doBody = foldr (uncurry Map.insert . doBlockCC . snd) Map.empty . bodyList

doBlockOX :: Block (Assigned Reg (Insn Reg)) O o -> [Asm.Line]
doBlockOX = foldBlockNodesB3
    ( \ _ -> id
    , (<|>) . doInsnOO
    , (<|>) . doInsnOC
    ) <*> blockShape & snd & join shape ([] :: [Asm.Line])

doBlockCC :: Block (Assigned Reg (Insn Reg)) C C -> (Label, [Asm.Line])
doBlockCC = foldBlockNodesB3
    (\ (Assigned (Argu (Const ())) (Label l)) -> (,) l
    , (<|>) . doInsnOO
    , (<|>) . doInsnOC
    ) `flip` []

doInsnOO :: Assigned Reg (Insn Reg) O O -> [Asm.Line]
doInsnOO (Assigned (Lhs Nothing) insn) = case insn of
    BumpStack a -> [Asm.Add (Asm.RegOperand Asm.Rsp) (doOperand a)]
    Write lsz a d -> [Asm.Mov Nothing (Asm.MemOperand lsz (doAddr a)) (doOperand d)]
    _ -> []
doInsnOO (Assigned (Lhs (Just reg)) insn) = case insn of
    BumpStack a
      | Local i <- a, reg == i ->
        [Asm.Add (Asm.RegOperand i) (Asm.RegOperand Asm.Rsp), Asm.Xchg x (Asm.RegOperand Asm.Rsp)]
      | otherwise ->
        [Asm.Mov Nothing x (Asm.RegOperand Asm.Rsp), Asm.Add (Asm.RegOperand Asm.Rsp) (doOperand a)]
    UnOp op a -> [(case op of
        Ham -> Asm.Popcnt
        Clz -> Asm.Lzcnt
        Ctz -> Asm.Tzcnt) x (doOperand a)]
    BinOp op a b
      | Local i <- a, reg == i -> doBinOp op x (doOperand b)
      | Local j <- b, reg == j -> doBinOp op x (doOperand a)
      | otherwise -> Asm.Mov Nothing x (doOperand a) : doBinOp op x (doOperand b)
    Read lsz a -> [Asm.Mov Nothing x (Asm.MemOperand lsz (doAddr a))]
    Write lsz a d -> [Asm.Mov Nothing (Asm.MemOperand lsz (doAddr a)) (doOperand d)]
  where
    x = Asm.RegOperand reg

doInsnOC :: Assigned Reg (Insn Reg) O C -> [Asm.Line]
doInsnOC (Assigned NoLhs insn) = case insn of
    Branch _ (Core.Const _) _ _ _ -> error "branch"
    Branch c (Core.Local i) b s t ->
        [Asm.Cmp (Asm.RegOperand i) (doOperand b),
         Asm.J (Just case c of
                    Equal -> Asm.E
                    Less False -> Asm.B
                    Less True  -> Asm.L) (mkLabelOperand t),
         Asm.J Nothing (mkLabelOperand s)]
    UBranch l -> [Asm.J Nothing (mkLabelOperand l)]
    Jump a as -> mkArguments (doOperand <$> as) ++ [Asm.J Nothing $ case a of
        Core.Local k -> Asm.RegOperand k
        Core.Const (Global (Right l)) -> mkLabelOperand l
        Core.Const (Literal n) -> Asm.ImmediateOperand (Asm.Immediate (fromIntegral n))
        Core.Const (Global (Left (Name name))) -> error ("name: " ++ Text.unpack name)]
    Unreachable -> []

mkLabelOperand :: Label -> Asm.Operand False
mkLabelOperand = Asm.ImmediateOperand . Asm.LabelRelValue

doOperand :: Operand Reg -> Asm.Operand False
doOperand = \ case
    Local k -> Asm.RegOperand k
    Core.Const (Literal n) -> Asm.ImmediateOperand (Asm.Immediate (fromIntegral n))
    Core.Const (Global (Right l)) -> Asm.ImmediateOperand (Asm.LabelRelValue l)
    Core.Const (Global (Left (Name name))) -> error ("name: " ++ Text.unpack name)

doAddr :: Operand Reg -> Asm.Addr
doAddr = \ case
    Local i -> Asm.nullAddr { Asm.baseReg = Just i }
    Core.Const (Literal n) -> Asm.nullAddr { Asm.displacement = Asm.Immediate (fromIntegral n) }
    Core.Const (Global (Right l)) -> Asm.nullAddr { Asm.displacement = Asm.LabelRelValue l }
    Core.Const (Global (Left (Name name))) -> error ("name: " ++ Text.unpack name)

doBinOp :: BinOp -> Asm.Operand True -> Asm.Operand r -> [Asm.Line]
doBinOp = fmap sequenceA . sequenceA . \ case
    Add    -> [Asm.Add]
    Sub    -> [Asm.Sub]
    Mul    -> [Asm.Imul]
    XMul   -> error "xmul"
    MulH _ -> error "mulh"
    XMulH  -> error "xmulh"
    Min s  -> [Asm.Cmp, Asm.Mov (Just $ bool Asm.A Asm.G s)]
    Max s  -> [Asm.Cmp, Asm.Mov (Just $ bool Asm.B Asm.L s)]
    And    -> [Asm.And]
    Or     -> [Asm.Or]
    Xor    -> [Asm.Xor]
    Andc   -> [Asm.Andn]
    Orc    -> [\ a _ -> Asm.Not a, Asm.And, \ a _ -> Asm.Not a]
    Xnor   -> [Asm.Xor, \ a _ -> Asm.Not a]
    Nand   -> [Asm.And, \ a _ -> Asm.Not a]
    Nor    -> [Asm.Or,  \ a _ -> Asm.Not a]
    Grev   -> error "grev"
    Gorc   -> error "gorc"
    Shfl   -> error "shfl"
    Shift (ShiftL) -> [Asm.Shl]
    Shift (ShiftR False) -> [Asm.Shr]
    Shift (ShiftR True) -> [Asm.Sar]
    Rotate (ShiftL) -> [Asm.Rol]
    Rotate (ShiftR ()) -> [Asm.Ror]
    Div _  -> error "div"
    Rem _  -> error "rem"
    Slt s  -> [Asm.Cmp, \ a _ -> Asm.Set (bool Asm.B Asm.L s) a]

mkSwap :: Asm.Operand True -> Asm.Operand True -> [Asm.Line]
mkSwap a b = [Asm.Xor a b, Asm.Xor b a, Asm.Xor a b]

mkArguments :: [Asm.Operand r] -> [Asm.Line]
mkArguments =
    zipWithRemaining (compose2 (Asm.Mov Nothing) Asm.RegOperand id) [Asm.Rax ..] & \ (ls, r) -> ls ++ case r of
    Just (Right as) ->
        Asm.Add (Asm.RegOperand Asm.Rsp)
                (Asm.ImmediateOperand (Asm.Immediate (fromIntegral $ 8*length as))) :
        [Asm.Mov Nothing (Asm.MemOperand (LogSize 3)
                  Asm.nullAddr { Asm.baseReg = Just Asm.Rsp
                               , Asm.displacement = Asm.Immediate (8*k) }) a
        | (k, a) <- count (toList as)]
    _ -> []
