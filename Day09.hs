module Main where

import Data.Set (Set)
import Data.Set qualified as Set

import Pos

type Rope = (Pos, [Pos])

type Move = (Direction, Int)

main :: IO ()
main = do
  moves <- map decode . lines <$> readFile "Day09.input"
  print $ solution1 moves -- 5902
  print $ solution2 moves -- 2445

newRope :: Int -> Rope
newRope n = ((0, 0), replicate n (0, 0))

solution1 :: [Move] -> Int
solution1 = Set.size . snd . run (newRope 1)

solution2 :: [Move] -> Int
solution2 = Set.size . snd . run (newRope 9)

decode :: String -> Move
decode (d:' ':l) = (read [d], read l)
decode s = error $ "Bad input: " <> show s

run :: Rope -> [Move] -> (Rope, Set Pos)
run start@(_, st) = go (Set.fromList st) start
  where
    go !vs r       []          = (r, vs)
    go !vs r       ((_, 0):ms) = go vs r ms
    go !vs (h, ts) ((d, n):ms) =
      let h'  = moveTo d h
          ts' = pullTheTail h' ts
      in go (Set.insert (last ts') vs) (h', ts') ((d, n - 1) : ms)

pullTheTail :: Pos -> [Pos] -> [Pos]
pullTheTail _ [] = []
pullTheTail h (t:ts) =
  let t' = pullTheSegment h t
  in t' : pullTheTail t' ts

pullTheSegment :: Pos -> Pos -> Pos
pullTheSegment (hx, hy) t@(tx, ty)
  | abs dx > 1 || abs dy > 1 = (tx + signum dx, ty + signum dy)
  | otherwise = t
  where
    dx = hx - tx
    dy = hy - ty

example :: [Move]
example = map decode
  [ "R 4"
  , "U 4"
  , "L 3"
  , "D 1"
  , "R 4"
  , "D 1"
  , "L 5"
  , "R 2"
  ]

bigExample :: [Move]
bigExample = map decode
  [ "R 4"
  , "U 4"
  , "L 3"
  , "D 1"
  , "R 4"
  , "D 1"
  , "L 5"
  , "R 2"
  ]
