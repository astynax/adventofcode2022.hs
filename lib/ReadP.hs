{-# OPTIONS -Wall #-}

module ReadP
  ( module P
  , intP
  , tryReadP
  ) where

import Data.Char
import Text.ParserCombinators.ReadP as P
import Control.Applicative ((<|>))

tryReadP :: String -> ReadP a -> String -> a
tryReadP name p s = case readP_to_S p s of
  ((x, "") : _) -> x
  _             -> error $ "Can't parse [" <> name <> "]!"

intP :: ReadP Int
intP = (negate <$> (char '-' *> natP)) <|> natP

natP :: ReadP Int
natP = read <$> munch1 isDigit
