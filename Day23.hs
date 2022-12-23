module Main where

import Data.List

import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Set (Set)
import Data.Set qualified as Set

import Pos

type Dirs = [Direction]

main :: IO ()
main = do
  e <- decode <$> readFile "Day23.input"
  print $ solution1 e -- 4218
  print $ solution2 e -- 976

decode :: String -> Set Pos
decode = Set.fromList . concatMap row . zip [0..] . lines
  where
    row (y, r) = [(x, y) | (x, c) <- zip [0..] r, c == '#']

solution1 :: Set Pos -> Int
solution1 elves =
  let e = run 10 elves
      ((x0, x1), (y0, y1)) = Pos.dimensions e
  in (x1 - x0 + 1) * (y1 - y0 + 1) - Set.size e

run :: Int -> Set Pos -> Set Pos
run n = go n (cycle [U,D,L,R])
  where
    go 0 _  s = s
    go i ds s =
      let (_, s', ds') = step ds s
      in go (i - 1) ds' s'

solution2 :: Set Pos -> Int
solution2 = go 1 (cycle [U,D,L,R])
  where
    go n ds s =
      let (done, s', ds') = step ds s
      in if done then n
         else go (n + 1) ds' s'

step :: Dirs -> Set Pos -> (Bool, Set Pos, Dirs)
step dirs elves =
  ( null moves
  , (elves `Set.difference` old) `Set.union` new
  , tail dirs )
  where
    old = Set.fromList $ map fst moves
    new = Set.fromList $ map snd moves
    moves = [(f, t) | (t, [f]) <- Map.toList intentions]
    intentions = foldl' try Map.empty $ Set.toList elves
    try m pos
      | isAlone pos = m
      | otherwise =
        case getFirst (foldMap (tryToGo elves pos) $ take 4 dirs) of
          Nothing -> m
          Just p  -> Map.insertWith (<>) p [pos] m
    isAlone pos =
      Set.null . Set.intersection elves . Set.fromList $ neibs8 pos

tryToGo :: Set Pos -> Pos -> Direction -> First Pos
tryToGo elves (x, y) dir =
  if Set.null (Set.intersection elves $ Set.fromList ns)
  then First (Just pos')
  else First Nothing
  where
    (pos', ns) = case dir of
      U -> ((x, y - 1), [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)])
      D -> ((x, y + 1), [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)])
      L -> ((x - 1, y), [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1)])
      R -> ((x + 1, y), [(x + 1, y - 1), (x + 1, y), (x + 1, y + 1)])

example :: Set Pos
example = decode $ unlines
  [ "....#.."
  , "..###.#"
  , "#...#.#"
  , ".#...##"
  , "#.###.."
  , "##.#.##"
  , ".#..#.."
  ]

animate :: Set Pos -> IO ()
animate = go (cycle [U,D,L,R])
  where
    go ds s = do
      drawSetOf (\_ f -> if f then '#' else '.') s
      _ <- getLine
      print $ head ds
      let (_, s', ds') = step ds s
      go ds' s'
