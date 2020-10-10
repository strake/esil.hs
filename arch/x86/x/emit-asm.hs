{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Main where

import Prelude hiding (lex)
import Compiler.Flow.Shape (MaybeC (..))
import Control.Arrow
import qualified Control.Monad.Free as Free
import Control.Monad.Trans.Except (runExcept)
import Data.Bool (bool)
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Class as MC
import qualified Data.Map.Fresh.Class as C (FreshMap)
import qualified Data.Map.Fresh.Class as MC (FreshMap (..))
import Data.Monoid (Alt (..))
import Data.Void (absurd)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc.Render.String
import Core.RegAlloc
import Text.Earley
import Text.Lexer (defaultLexerSpec)
import qualified Text.Lexer
import qualified Text.Regex.Applicative as RE
import Util

import Core hiding (Const)
import qualified Parse
import qualified Asm
import qualified AsmGen as Asm

main :: IO ()
main = interact $ doParse & \ case
    (a:_, _) -> case runExcept $ (traverse . traverse) (allocRegs 15 NothingC) a of
        Left e -> error (show e)
        Right a' ->
            renderString $ layoutPretty LayoutOptions { layoutPageWidth = AvailablePerLine 96 (7/8) } $
            Map.foldMapWithKey (fmap pretty . uncurry . Fn . unName) . fmap (either absurd Asm.doGraphOC) $
            (fmap . fmap) (bimapGraphBinders (toReg . snd) (toReg . snd)) $ a'

    (_, r) -> error (show r)

toReg n = toEnum (n - bool 1 0 (n >= 4))

doParse :: _ -> ([Map _ _], _)
doParse = fullParses theParser . lex

lex = fmap snd . fst . listFree . Text.Lexer.lex defaultLexerSpec { Text.Lexer.token = Parse.token, Text.Lexer.space = () <$ RE.psym ((==) Space . generalCategory) }

listFree = getAlt *** id <<< Free.fold (\ (p, t, a) -> (Alt [(p, t)], a))

theParser = parser Parse.grammar

data Fn = Fn
  { name :: Text
  , entry :: [Asm.Line]
  , body :: Map Core.Label [Asm.Line]
  }

instance Pretty Fn where
    pretty (Fn { name, entry, body }) = (mconcat . fmap (<> Pretty.line))
        (pretty name <> ":" :
         fmap (("\t" <>) . pretty) entry ++
         MC.foldrWithKey (\ (Lbl l) cs -> (++) $ ("." <> pretty l <> ":") : fmap pretty cs) [] body)

instance C.FreshMap (Map Label) where
    insertFreshWithKeyF f as = (\ a -> (l, Map.insert l a as)) <$> f l
      where
        l = case Map.lookupMax as of
            Nothing -> Lbl 0
            Just (Lbl n, _) -> Lbl (n+1)
