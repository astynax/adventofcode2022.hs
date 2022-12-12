module Pos
  ( Pos
  , Direction(..)
  , moveTo, whichDirection
  , drawSetOf
  , levi
  , neibs
  ) where

import Data.Set (Set)
import Data.Set qualified as Set

type Pos = (Int, Int)

data Direction = U | D | L | R deriving (Eq, Show, Read)

moveTo :: Direction -> Pos -> Pos
moveTo d (x, y) = case d of
  U -> (x, y + 1)
  D -> (x, y - 1)
  L -> (x - 1, y)
  R -> (x + 1, y)

whichDirection :: Pos -> Pos -> Maybe Direction
whichDirection (x1, y1) (x2, y2) =
  case (x2 - x1, y2 - y1) of
    ( 1, 0) -> Just R
    (-1, 0) -> Just L
    (0,  1) -> Just D
    (0, -1) -> Just U
    _       -> Nothing

drawSetOf :: (Bool -> Char) -> Set Pos -> IO ()
drawSetOf chunk s = mapM_ row [minimum ys .. maximum ys]
  where
    row y = mapM_ (cell y) [minimum xs .. maximum xs] >> putChar '\n'
    cell y x = putChar . chunk $ Set.member (x, y) s
    ps = Set.toList s
    xs = map fst ps
    ys = map snd ps

levi :: Pos -> Pos -> Int
levi (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

neibs :: Pos -> [Pos]
neibs (x, y) =
  [ (x, y - 1)
  , (x, y + 1)
  , (x - 1, y)
  , (x + 1, y)
  ]
