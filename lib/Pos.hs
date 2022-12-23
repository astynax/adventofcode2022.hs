module Pos
  ( Pos
  , Direction(..)
  , Turn(..), turn
  , moveTo, moveToFlipped, whichDirection
  , drawSetOf
  , manhattan
  , neibs, neibs8
  , fill
  , dimensions
  ) where

import Data.Set (Set)
import Data.Set qualified as Set

type Pos = (Int, Int)

data Direction = U | D | L | R deriving (Eq, Show, Read)
data Turn = TR | TL deriving Show

moveToFlipped :: Direction -> Pos -> Pos
moveToFlipped d (x, y) = case d of
  U -> (x, y + 1)
  D -> (x, y - 1)
  L -> (x - 1, y)
  R -> (x + 1, y)

moveTo :: Direction -> Pos -> Pos
moveTo d (x, y) = case d of
  U -> (x, y - 1)
  D -> (x, y + 1)
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

drawSetOf :: (Pos -> Bool -> Char) -> Set Pos -> IO ()
drawSetOf chunk s = mapM_ row (uncurry range ry)
  where
    row y = mapM_ (cell y) (uncurry range rx) >> putChar '\n'
    cell y x = putChar . chunk (x, y) $ ((x, y) `Set.member` s)
    (rx, ry) = dimensions s

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

neibs :: Pos -> [Pos]
neibs (x, y) =
  [ (x, y - 1)
  , (x, y + 1)
  , (x - 1, y)
  , (x + 1, y)
  ]

neibs8 :: Pos -> [Pos]
neibs8 (x, y) =
  neibs (x, y) <>
  [ (x - 1, y - 1)
  , (x + 1, y + 1)
  , (x - 1, y + 1)
  , (x + 1, y - 1)
  ]

fill :: Pos -> Pos -> [Pos]
fill (x1, y1) (x2, y2) = (,) <$> range x1 x2 <*> range y1 y2

range :: Int -> Int -> [Int]
range f t
  | f < t     = [f .. t]
  | otherwise = [t .. f]

dimensions :: Set Pos -> (Pos, Pos)
dimensions s =
  ( (minimum xs, maximum xs)
  , (minimum ys, maximum ys) )
  where
    ps = Set.toList s
    xs = map fst ps
    ys = map snd ps

turn :: Turn -> Direction -> Direction
turn TL U = L
turn TL L = D
turn TL D = R
turn TL R = U
turn TR U = R
turn TR R = D
turn TR D = L
turn TR L = U
