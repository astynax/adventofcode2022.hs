{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (foldl')

import Data.Set (Set)
import Data.Set qualified as Set

import Megaparsec

type Pos3D = (Int, Int, Int)
type Dimensions = ((Int, Int), (Int, Int), (Int, Int))

main :: IO ()
main = do
  print $ solution example -- (64,58)
  xs <- decode <$> readFile "Day18.input"
  print $ solution xs -- (3530,2000)

decode :: String -> [Pos3D]
decode = tryParse inputP "input"

inputP :: Parser [Pos3D]
inputP = many $ lineP <* "\n"
  where
    lineP = (,,) <$> decimal <*> ("," *> decimal) <*> ("," *> decimal)

solution :: [Pos3D] -> (Int, Int)
solution xs =
  let s = Set.fromList xs
      a = areaOf s
      ps = pockets xs
  in (a, a - areaOf ps)

areaOf :: Set Pos3D -> Int
areaOf s =
  6 * Set.size s - sum
  [ Set.size $ Set.intersection (Set.fromList $ neibs x) s
  | x <- Set.toList s ]

pockets :: [Pos3D] -> Set Pos3D
pockets input =
  Set.difference volume . Set.union s $ go Set.empty $ Set.singleton start
  where
    s = Set.fromList input
    dims@(dx, dy, dz) = dimensions input
    range = uncurry enumFromTo
    volume = Set.fromList [ p | x <- range dx
                              , y <- range dy
                              , z <- range dz
                              , let p = (x, y, z) ]
    start = head . Set.toList $ Set.difference volume s
    go !vs ps
      | Set.null ps = vs
      | otherwise   =
        uncurry go $ foldl' step (vs, Set.empty) ps
    step (!vs, ps) pos = (Set.insert pos vs, ps <> ns)
      where
        ns =
          Set.fromList
          [ p | p <- neibs pos
              , p `inside` dims
              , not $ p `Set.member` vs
              , not $ p `Set.member` s
              ]

neibs :: Pos3D -> [Pos3D]
neibs (x, y, z) =
  [ (x - 1, y,     z)
  , (x + 1, y,     z)
  , (x,     y - 1, z)
  , (x,     y + 1, z)
  , (x,     y,     z - 1)
  , (x,     y,     z + 1)
  ]

dimensions :: [Pos3D] -> Dimensions
dimensions ps =
  ( dims \(x, _, _) -> x
  , dims \(_, y, _) -> y
  , dims \(_, _, z) -> z )
  where
    dims f = let vs = map f ps in (minimum vs, maximum vs)

inside :: Pos3D -> Dimensions -> Bool
inside (x, y, z) ((minX, maxX), (minY, maxY), (minZ, maxZ)) =
  minX <= x && x <= maxX &&
  minY <= y && y <= maxY &&
  minZ <= z && z <= maxZ

example :: [Pos3D]
example =
  [ (2, 2, 2)
  , (1, 2, 2)
  , (3, 2, 2)
  , (2, 1, 2)
  , (2, 3, 2)
  , (2, 2, 1)
  , (2, 2, 3)
  , (2, 2, 4)
  , (2, 2, 6)
  , (1, 2, 5)
  , (3, 2, 5)
  , (2, 1, 5)
  , (2, 3, 5)
  ]
