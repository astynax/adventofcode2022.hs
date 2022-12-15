module Megaparsec
  ( module MP
  , Parser
  , tryParse
  , integerP
  ) where

import Text.Megaparsec as MP hiding (Pos)
import Text.Megaparsec.Char as MP
import Text.Megaparsec.Char.Lexer as MP (decimal)
import Data.Void (Void)

type Parser = Parsec Void String

tryParse
  :: (VisualStream s, TraversableStream s, ShowErrorComponent e)
     => Parsec e s c -> String -> s -> c
tryParse p name = either boom id . parse p name
  where
    boom e = error $ errorBundlePretty e

integerP :: Parser Int
integerP = negate <$> (char '-' *> decimal) <|> decimal
