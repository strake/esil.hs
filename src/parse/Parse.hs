{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
module Parse where

import qualified Compiler.Flow.Block as Block
import Compiler.Flow.Graph (Graph)
import qualified Compiler.Flow.Graph as Graph
import Compiler.Flow.NonLocal (NonLocal (..))
import Compiler.Flow.Shape (End (..), MaybeO (..), SEnd (..))
import Control.Applicative
import Data.Foldable
import Data.Map.Class (Map)
import qualified Data.Map.Class as Map
import Data.Text (Text)
import Numeric.Natural
import Text.Earley.Grammar (Grammar, Prod, rule)
import qualified Text.Earley as P
import qualified Text.Earley.Grammar as P
import Text.Regex.Applicative (RE)
import qualified Text.Regex.Applicative as RE
import qualified Text.Regex.Applicative.Lex as RE

import Core hiding (Const, Equal)
import qualified Core
import Data.Assignment

data Token = LParenth | RParenth | Colon | Equal | Word Text | Number Natural | Percent | LineBreak
  deriving (Eq, Show)

token :: RE Char Token
token = asum
  [ LParenth <$ RE.sym '('
  , RParenth <$ RE.sym ')'
  , Colon <$ RE.sym ':'
  , Equal <$ RE.sym '='
  , Word <$> RE.ident'
  , Number <$> RE.natural'
  , Percent <$ RE.sym '%'
  , LineBreak <$ RE.sym '\n'
  ]

grammar
 :: (Map map, Map.Key map ~ Name, Map map', Map.Key map' ~ Core.Label)
 => Grammar r (Prod r Text Token (map (Either a (Graph map' (Assigned SrcBndr (Insn SrcBndr)) O C))))
grammar = mdo
    decls <- rule $ many (fmap Left <$> static <|> fmap Right <$> fn)
    fn <- rule $ (,) <$ P.token (Word "fn") <*> P.terminal (\ case Word x -> Just (Name x); _ -> Nothing) <* P.token LineBreak <*> body'
    static <- rule $ (,) <$ P.token (Word "static") <*> P.terminal (\ case Word x -> Just (Name x); _ -> Nothing) <*> empty
    body' <- rule $ Graph.Body . JustO <$> blockOC <*> body <*> pure NothingO
    let body = foldr (Map.insert =<< entryLabel) Map.empty <$> many blockCC
    blockCC <- rule $ Block.cons Clos <$> label <*> blockOC
    blockOC <- rule $ (flip . foldr) (Block.cons Open) <$> many insnOO <*> (Block.snoc Clos Block.empty <$> insnOC)
    insnOO <- rule $ Assigned . Lhs <$> optional (P.token Percent *> binder <* P.token Equal) <*>
      asum
      [ BumpStack <$ P.token (Word "bump-stack") <*> operand
      , Read <$ P.token (Word "read") <*> logSize <*> operand
      , Write <$ P.token (Word "write") <*> logSize <*> operand <*> operand
      , UnOp <$> unOp <*> operand
      , BinOp <$> binOp <*> operand <*> operand
      ] <* P.token LineBreak P.<?> "instruction"
    insnOC <- rule $ Assigned NoLhs <$>
      asum
      [ Unreachable <$ P.token (Word "unreachable")
      , Branch <$ P.token (Word "br") <*> branchCmp <*> operand <*> operand <*> label' <*> label'
      , UBranch <$ P.token (Word "br") <*> label'
      , Jump <$ P.token (Word "jump") <*> operand <*> many operand
      ] <* P.token LineBreak P.<?> "instruction — terminator"
    operand <- rule $ Local <$ P.token Percent <*> binder <|> Core.Const <$> constant P.<?> "operand"
    constant <- rule $ (P.<?> "constant") $ P.terminal \ case Number n -> Just (Literal n); _ -> Nothing
    label <- rule $ (Assigned (Argu (Const ())) . Label) <$> label' <* traverse_ P.token [Colon, LineBreak]
    label' <- rule $ (P.<?> "label") $ Lbl <$> P.terminal \ case
        Number n -> Just (fromIntegral n)
        _ -> Nothing
    let binder = P.terminal \ case
            Number n -> Just (Left n)
            Word x -> Just (Right x)
            _ -> Nothing
    logSize <- rule $ P.terminal \ case
        Number n -> Just (LogSize (fromIntegral n))
        _ -> Nothing
    pure (Map.fromList <$> decls)

unOp :: Prod r e Token UnOp
unOp = P.terminal \ case
    Word "ham" -> Just Ham
    Word "clz" -> Just Clz
    Word "ctz" -> Just Ctz
    _ -> Nothing

binOp :: Prod r e Token BinOp
binOp = P.terminal \ case
    Word "add"    -> Just $ Add
    Word "sub"    -> Just $ Sub
    Word "and"    -> Just $ And
    Word "or"     -> Just $ Or
    Word "xor"    -> Just $ Xor
    Word "andc"   -> Just $ Andc
    Word "orc"    -> Just $ Orc
    Word "xnor"   -> Just $ Xnor
    Word "nand"   -> Just $ Nand
    Word "nor"    -> Just $ Nor
    Word "grev"   -> Just $ Grev
    Word "gorc"   -> Just $ Gorc
    Word "shfl"   -> Just $ Shfl
    Word "shr"    -> Just $ Shift (ShiftR True)
    Word "shru"   -> Just $ Shift (ShiftR False)
    Word "shl"    -> Just $ Shift (ShiftL)
    Word "ror"    -> Just $ Rotate (ShiftR ())
    Word "rol"    -> Just $ Rotate (ShiftL)
    Word "min"    -> Just $ Min True
    Word "minu"   -> Just $ Min False
    Word "max"    -> Just $ Max True
    Word "maxu"   -> Just $ Max False
    Word "slt"    -> Just $ Slt True
    Word "sltu"   -> Just $ Slt False
    Word "mul"    -> Just $ Mul
    Word "mulh"   -> Just $ MulH SS
    Word "mulhu"  -> Just $ MulH UU
    Word "mulhsu" -> Just $ MulH SU
    Word "xmul"   -> Just $ XMul
    Word "xmulh"  -> Just $ XMulH
    Word "div"    -> Just $ Div True
    Word "divu"   -> Just $ Div False
    Word "rem"    -> Just $ Rem True
    Word "remu"   -> Just $ Rem False
    _ -> Nothing

branchCmp :: Prod r e Token BranchCmp
branchCmp = P.terminal \ case
    Word "eq" -> Just Core.Equal
    Word "l"  -> Just (Core.Less True)
    Word "lu" -> Just (Core.Less False)
    _ -> Nothing

type SrcBndr = Either Natural Text
