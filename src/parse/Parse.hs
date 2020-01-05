{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
module Parse where

import Compiler.Hoopl (Graph, Graph' (..), O, C, MaybeO (..), addBlock)
import qualified Compiler.Hoopl.Block as Block
import Compiler.Hoopl.Label
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

grammar :: (Map map, Map.Key map ~ Name) => Grammar r (Prod r Text Token (map (Either a (Graph (Assigned SrcBndr (Insn SrcBndr)) O C))))
grammar = mdo
    decls <- rule $ many (fmap Left <$> static <|> fmap Right <$> fn)
    fn <- rule $ (,) <$ P.token (Word "fn") <*> P.terminal (\ case Word x -> Just (Name x); _ -> Nothing) <* P.token LineBreak <*> body'
    static <- rule $ (,) <$ P.token (Word "static") <*> P.terminal (\ case Word x -> Just (Name x); _ -> Nothing) <*> empty
    body' <- rule $ GMany . JustO <$> blockOC <*> body <*> pure NothingO
    let body = foldr addBlock Map.empty <$> many blockCC
    blockCC <- rule $ Block.joinHead <$> label <*> blockOC
    blockOC <- rule $ (flip . foldr) Block.cons <$> many insnOO <*> (Block.joinTail Block.empty <$> insnOC)
    insnOO <- rule $ Assigned . Lhs <$> optional (P.token Percent *> binder <* P.token Equal) <*>
      asum
      [ BumpStack <$ P.token (Word "bump-stack") <*> operand
      ] <* P.token LineBreak P.<?> "instruction"
    insnOC <- rule $ Assigned NoLhs <$>
      asum
      [ Unreachable <$ P.token (Word "unreachable")
      ] <* P.token LineBreak P.<?> "instruction — terminator"
    operand <- rule $ Local <$ P.token Percent <*> binder <|> Core.Const <$> constant P.<?> "operand"
    constant <- rule $ (P.<?> "constant") $ P.terminal \ case Number n -> Just (Literal n); _ -> Nothing
    label <- rule $ (Assigned (Argu (Const ())) . Label) <$> label' <* traverse_ P.token [Colon, LineBreak]
    label' <- rule $ (P.<?> "label") $ uniqueToLbl <$> P.terminal \ case
        Number n -> Just (fromIntegral n)
        _ -> Nothing
    let binder = P.terminal \ case
            Number n -> Just (Left n)
            Word x -> Just (Right x)
            _ -> Nothing
    pure (Map.fromList <$> decls)

type SrcBndr = Either Natural Text
