{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.List
import Data.Monoid

import Megaparsec

data C4 a = C4 !a !a !a !a deriving Show

instance Semigroup a => Semigroup (C4 a) where
  C4 a b c d <> C4 x y z w = C4 (a <> x) (b <> y) (c <> z) (d <> w)

instance Monoid a => Monoid (C4 a) where
  mempty = C4 mempty mempty mempty mempty

type S4 = C4 (Sum Int)
newtype Blueprint = Blueprint (C4 Cost) deriving Show
newtype Stash = Stash S4 deriving Show
newtype Cost = Cost S4 deriving Show
newtype Robot = Robot S4 deriving (Show, Semigroup, Monoid)

main :: IO ()
main = do
  bps <- decode <$> readFile "Day19.input"
  print $ solution1 bps -- 1404
  print $ solution2 bps -- 5880

solution1 :: [Blueprint] -> Int
solution1 = sum . zipWith (*) [1..] . map (bfs 24)

solution2 :: [Blueprint] -> Int
solution2 = product . map (bfs 32) . take 3

decode :: String -> [Blueprint]
decode = tryParse inputP "input"

-- Blueprint 1: Each ore robot costs 4 ore. \
-- Each clay robot costs 4 ore. \
-- Each obsidian robot costs 4 ore and 8 clay. \
-- Each geode robot costs 2 ore and 15 obsidian.
inputP :: Parser [Blueprint]
inputP = many $ lineP <* "\n"
  where
    lineP :: Parser Blueprint
    lineP = do
      _   <- "Blueprint " *> (decimal :: Parser Int)
      ore <- ": Each ore robot costs "      *> costP
      cla <- ". Each clay robot costs "     *> costP
      obs <- ". Each obsidian robot costs " *> costP
      geo <- ". Each geode robot costs "    *> costP
      _   <- "."
      pure . Blueprint $ C4 ore cla obs geo
    costP = Cost . mconcat <$> matP `sepBy` " and "
    matP :: Parser S4
    matP = flip ($) <$> (negate <$> decimal) <*> (
      (\x -> C4 x mempty mempty mempty) <$ " ore" <|>
      (\x -> C4 mempty x mempty mempty) <$ " clay" <|>
      (\x -> C4 mempty mempty x mempty) <$ " obsidian"
      )

build :: Blueprint -> Stash -> [(Robot, Stash)]
build (Blueprint cs) (Stash s) = reverse
  [ (r, Stash s')
  | (Cost c, r) <- zip (toList cs)
    [ Robot (C4 1 0 0 0)
    , Robot (C4 0 1 0 0)
    , Robot (C4 0 0 1 0)
    , Robot (C4 0 0 0 1)
    ]
  , let s' = s <> c
  , all (>= 0) $ toList s'
  ]

getIncome :: Robot -> Stash -> Stash
getIncome (Robot r) (Stash s) = Stash $ s <> r

toList :: C4 a -> [a]
toList (C4 a b c d) = [a, b, c, d]

bfs :: Int -> Blueprint -> Int
bfs minutes bp = go minutes [(Robot (C4 1 0 0 0), Stash mempty)]
  where
    go !t plan
      | t == 0    =
        case plan of
          ((_, Stash (C4 _ _ _ (Sum o))) : _) -> o
          _ -> error "Impossible"
      | otherwise =
        go (t - 1) . take 1000 . sortOn key $ concatMap fork plan
    key (r, s) =
      let Stash (C4 o c d g) = getIncome r s
      in negate (g * 1000000 + d * 10000 + c * 100 + o)
    fork (robots, stash) =
      (robots, getIncome robots stash)
      : [ (robots <> r, getIncome robots s)
        | (r, s) <- build bp stash ]
