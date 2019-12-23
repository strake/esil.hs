{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Prelude hiding (lex)
import Compiler.Hoopl
import Control.Applicative
import Control.Arrow
import qualified Control.Monad.Free as Free
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Alt (..))
import Data.Void (absurd)
import Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc.Render.String
import Text.Earley
import Text.Lexer (defaultLexerSpec)
import qualified Text.Lexer
import qualified Text.Regex.Applicative as RE
import Util

import Core hiding (Const)
import qualified Parse

main :: IO ()
main = interact $ doParse & \ case
    (a:_, _) ->
        renderString $ layoutPretty LayoutOptions { layoutPageWidth = AvailablePerLine 96 (7/8) } $
        Map.foldMapWithKey (\ name body -> vsep ["fn" Pretty.<+> pretty name, pretty body, mempty]) . fmap (either absurd FnBody) $ a
    (_, r) -> error (show r)

doParse :: _ -> ([Map _ _], _)
doParse = fullParses theParser . lex

lex = fmap snd . fst . listFree . Text.Lexer.lex defaultLexerSpec { Text.Lexer.token = Parse.token, Text.Lexer.space = () <$ RE.psym ((==) Space . generalCategory) }

listFree = getAlt *** id <<< Free.fold (\ (p, t, a) -> (Alt [(p, t)], a))

theParser = parser Parse.grammar

instance (∀ i o . Pretty (n i o)) => Pretty (FnBody n) where
    pretty = Pretty.vsep . getAlt . getConst . traverseGraph (Const . pure . pretty) . fnBody

newtype FnBody n = FnBody { fnBody :: Graph n O C }
