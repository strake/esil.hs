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

import Core

data Token = LParenth | RParenth | Colon | Word Text | Number Natural | Percent | LineBreak
  deriving (Eq, Show)

token :: RE Char Token
token = asum
  [ LParenth <$ RE.sym '('
  , RParenth <$ RE.sym ')'
  , Colon <$ RE.sym ':'
  , Word <$> RE.ident'
  , Number <$> RE.natural'
  , Percent <$ RE.sym '%'
  , LineBreak <$ RE.sym '\n'
  ]

grammar :: (Map map, Map.Key map ~ Name) => Grammar r (Prod r Token Token (map (Either a (Graph (Insn (Either Natural Text)) O C))))
grammar = mdo
    decls <- rule $ many (fmap Left <$> static <|> fmap Right <$> fn)
    fn <- rule $ (,) <$ P.namedToken (Word "fn") <*> P.terminal (\ case Word x -> Just (Name x); _ -> Nothing) <* P.namedToken LineBreak <*> body'
    static <- rule $ (,) <$ P.namedToken (Word "static") <*> P.terminal (\ case Word x -> Just (Name x); _ -> Nothing) <*> empty
    body' <- rule $ GMany . JustO <$> blockOC <*> body <*> pure NothingO
    let body = foldr addBlock Map.empty <$> many blockCC
    blockCC <- rule $ Block.joinHead <$> label <*> blockOC
    blockOC <- rule $ (flip . foldr) Block.cons <$> many insnOO <*> (Block.joinTail Block.empty <$> insnOC)
    insnOO <- rule $
      asum
      [ BumpStack <$ P.namedToken (Word "bump-stack") <*> operand
      ] <* P.namedToken LineBreak
    insnOC <- rule $
      asum
      [ Unreachable <$ P.namedToken (Word "unreachable")
      ] <* P.namedToken LineBreak
    operand <- rule $
      Local <$ P.namedToken Percent <*> P.terminal (\ case
          Number n -> Just (Left n)
          Word x -> Just (Right x)
          _ -> Nothing) <|> Core.Const <$> constant
    constant <- rule $
      P.terminal (\ case Number n -> Just (Literal n); _ -> Nothing)
    label <- rule $ (Label . uniqueToLbl) <$> P.terminal (\ case Number n -> Just (fromIntegral n); _ -> Nothing) <* P.namedToken Colon <* P.namedToken LineBreak
    pure (Map.fromList <$> decls)
