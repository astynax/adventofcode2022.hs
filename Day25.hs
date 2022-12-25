{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Text.Read hiding ((<++))
import Text.ParserCombinators.ReadP

newtype Snafu = Snafu Int deriving Num

instance Read Snafu where
  readPrec = lift $ Snafu . fromSnafuDigits <$> many1 digitP
    where
      digitP =
        ((-2) <$ char '=') <++
        ((-1) <$ char '-') <++
        (0    <$ char '0') <++
        (1    <$ char '1') <++
        (2    <$ char '2')

instance Show Snafu where
  show (Snafu i) = toSnafuDigits i

main :: IO ()
main = do
  xs <- map (read @Snafu) . lines <$> readFile "Day25.input"
  print $ sum xs

fromSnafuDigits :: [Int] -> Int
fromSnafuDigits = sum . zipWith (*) ps . reverse
  where
    ps = map (5 ^) [0 :: Int ..]

toSnafuDigits :: Int -> String
toSnafuDigits 0 = "0"
toSnafuDigits v = reverse $ go 0 v
  where
    go 0 0 = ""
    go c 0 = go 0 c
    go c n =
      let (a, b) = (n + c) `divMod` 5
      in case b of
        4 -> '-' : go 1 a
        3 -> '=' : go 1 a
        2 -> '2' : go 0 a
        1 -> '1' : go 0 a
        0 -> '0' : go 0 a
        _ -> error "impossible"
