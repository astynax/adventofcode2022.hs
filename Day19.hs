{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Monoid

import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map

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
  es <- decode <$> readFile "Day19.example"
  -- print . zip [1 :: Int ..] $ map (simulate 24) es
  -- -- [(1,9),(2,12)]
  print $ map (simulate 32) es
  -- -- [42, 62] FIXME: should be [56, 62]

  bps <- decode <$> readFile "Day19.input"
  -- print . zip [1 :: Int ..] $ map (simulate 24) bps
  print . map (simulate 32) $ take 3 bps

  print . sum $ map (uncurry (*)) firstResults -- 1404
  print $ product secondResults                -- 31

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

simulate :: Int -> Blueprint -> Int
simulate minutes bp =
  (Map.! 0)
  $ go minutes (Robot (C4 1 0 0 0)) (Stash mempty)
  `execState` Map.empty
  where
    go !t !robots stash@(Stash (C4 (Sum o) _ _ (Sum s)))
      | t == 0    = modify $ Map.insertWith max t s
      | otherwise = do
        continue <- gets (Map.lookup t) >>= \case
          Nothing -> pure True
          Just p  -> pure $ p <= s
        when continue do
          modify $ Map.insert t s
          sequence_
            [ go (t - 1) (robots <> r) (getIncome robots s')
            | (r, s') <- build bp stash ]
          when (o < 6)
            $ go (t - 1) robots (getIncome robots stash)

firstResults :: [(Int, Int)]
firstResults =
  [ (1, 1), (2, 0), (3, 4), (4, 5), (5, 5)
  , (6, 4), (7, 1), (8, 1), (9, 8), (10, 6)
  , (11, 3), (12, 15), (13, 1), (14, 5), (15, 9)
  , (16, 3), (17, 0), (18, 5), (19, 0), (20, 6)
  , (21, 1), (22, 0), (23, 0), (24, 6), (25, 2)
  , (26, 6), (27, 0), (28, 1), (29, 3), (30, 0)
  ]

secondResults :: [Int]
secondResults =
  [ 21
  , 7
  , 40 -- FIXME: why it is 31 now?
  ]
