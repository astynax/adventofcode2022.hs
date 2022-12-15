{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (foldl')
import Control.Monad (guard)

import Data.Set (Set)
import Data.Set qualified as Set

import Megaparsec hiding (Pos)
import Pos

type Path = [Pos]
type Input = [Path]
type Cave = Set Pos

main :: IO ()
main = do
  inp <- toCave . tryParse inputP "input" <$> readFile "Day14.input"
  print $ solution1 inp -- 1199
  print $ solution2 inp -- 23925

source :: Pos
source = (500, 0)

inputP :: Parser Input
inputP = lineP `sepBy` "\n"
  where
    lineP = posP `sepBy` " -> "
    posP = (,) <$> decimal <* "," <*> decimal

trace :: Path -> Path
trace = (concat .) . zipWith fill <*> tail

toCave :: Input -> Cave
toCave = foldl' (flip Set.insert) Set.empty . concatMap trace

dropOne :: (Pos -> Cave -> Maybe Cave) -> Int -> Cave -> Maybe Cave
dropOne handleFloor floorLevel !cave = go source
  where
    probe pos = pos <$ guard (not $ pos `Set.member` cave)
    go pos@(x, y)
      | y > floorLevel = handleFloor pos cave
      | otherwise =
        probe pos >> maybe (Just $ Set.insert pos cave) go (
          probe (x,     y + 1) <|>
          probe (x - 1, y + 1) <|>
          probe (x + 1, y + 1) )

solution1 :: Cave -> Int
solution1 = run \_ _ -> Nothing

solution2 :: Cave -> Int
solution2 = run $ (Just .) . Set.insert

run :: (Pos -> Cave -> Maybe Cave) -> Cave -> Int
run handleFloor start = go 0 start
  where
    floorLevel = maxY start
    go n cave = case dropOne handleFloor floorLevel cave of
      Nothing -> n
      Just c' -> go (n + 1) c'

example :: Cave
example = toCave . tryParse inputP "example" $ unlines
  [ "498,4 -> 498,6 -> 496,6"
  , "503,4 -> 502,4 -> 502,9 -> 494,9"
  ]

maxY :: Cave -> Int
maxY = maximum . map snd . Set.toList

draw :: Cave -> IO ()
draw = drawSetOf \_ x -> if x then '#' else '.'

animate :: (Pos -> Cave -> Maybe Cave) -> Cave -> IO ()
animate handleFloor start = go start
  where
    floorLevel = maxY start
    go cave = do
      draw cave
      _ <- putStrLn "---" >> getChar
      case dropOne handleFloor floorLevel cave of
        Nothing -> pure ()
        Just c' -> go c'
