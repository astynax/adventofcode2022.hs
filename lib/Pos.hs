module Pos
  ( Pos
  , Direction(..)
  , moveTo
  , drawSetOf
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

drawSetOf :: (Bool -> Char) -> Set Pos -> IO ()
drawSetOf chunk s = mapM_ row [minimum ys .. maximum ys]
  where
    row y = mapM_ (cell y) [minimum xs .. maximum xs] >> putChar '\n'
    cell y x = putChar . chunk $ Set.member (x, y) s
    ps = Set.toList s
    xs = map fst ps
    ys = map snd ps
