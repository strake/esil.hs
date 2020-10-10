{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Main where

import Prelude hiding (Functor, (<$>), lex, map)
import Compiler.Flow.Graph (Graph)
import qualified Compiler.Flow.Graph as Graph
import Compiler.Flow.Shape (End (..))
import Control.Applicative hiding ((<$>))
import Control.Arrow
import qualified Control.Monad.Free as Free
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Alt (..))
import Data.Void (absurd)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc.Render.String
import Text.Earley
import Text.Lexer (defaultLexerSpec)
import qualified Text.Lexer
import qualified Text.Regex.Applicative as RE
import Util

import Core hiding (Const)
import Data.Assignment
import qualified Parse

main :: IO ()
main = interact $ doParse & \ case
    (a:_, _) ->
        renderString $ layoutPretty LayoutOptions { layoutPageWidth = AvailablePerLine 96 (7/8) } $
        Map.foldMapWithKey (\ name body -> vsep ["fn" Pretty.<+> pretty name, pretty body, mempty]) .
        fmap (either absurd ((FnBody :: _ -> FnBody (Map Label) _) . mapGraphBinders (Text.pack . show ||| id))) $ a
    (_, r) -> error (show r)

doParse :: _ -> ([Map _ _], _)
doParse = fullParses theParser . lex

lex = fmap snd . fst . listFree . Text.Lexer.lex defaultLexerSpec { Text.Lexer.token = Parse.token, Text.Lexer.space = () <$ RE.psym ((==) Space . generalCategory) }

listFree = getAlt *** id <<< Free.fold (\ (p, t, a) -> (Alt [(p, t)], a))

theParser = parser Parse.grammar

instance (∀ i o . Pretty (n i o), Traversable map) => Pretty (FnBody map n) where
    pretty = Pretty.vsep . getAlt . getConst . Graph.traverse' (Const . pure . pretty) . fnBody

instance {-# OVERLAPPING #-} (∀ i o . Pretty (n i o), Pretty k, Traversable map) => Pretty (FnBody map (Assigned k n)) where
    pretty = Pretty.vsep . getAlt . getConst . Graph.traverse' (Const . pure . prettyAssigned) . fnBody

prettyAssigned :: (Pretty k, Pretty (n i o)) => Assigned k n i o -> Doc a
prettyAssigned (Assigned lhs rhs) = Pretty.hsep (lhsDoc ++ [pretty rhs])
  where
    lhsDoc = case lhs of
        Lhs (Just k) -> ["%" <> pretty k, "="]
        _ -> []

newtype FnBody map n = FnBody { fnBody :: Graph map n O C }
