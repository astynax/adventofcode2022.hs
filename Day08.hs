module Main where

import Data.List

main :: IO ()
main = do
  xs <- map fromLine . lines <$> readFile "Day08.input"
  print $ solution1 xs -- 1823
  print $ solution2 xs -- 211680

solution1 :: [[Int]] -> Int
solution1 []      = 0 -- impossible
solution1 f@(r:_) = perimeter + sum [1 | xs <- checkVisible f, x <- xs, x]
  where
    perimeter = 2 * (length f + length r) - 4

solution2 :: [[Int]] -> Int
solution2 = maximum . concat . scores

fromLine :: String -> [Int]
fromLine = map (read . (:[]))

lookLeftRight :: [a] -> [(a, [a], [a])]
lookLeftRight = map chop . init . (zip <$> inits <*> tails)
  where
    chop (ls, r:rs) = (r, reverse ls, rs)
    chop _          = error "ipossible"

lookAround :: [[a]] -> [[(a, [[a]])]]
lookAround =
  zipWith (zipWith merge)
  <$> map lookLeftRight
  <*> (transpose . map lookLeftRight . transpose)
  where
    merge (v, ls, rs) (_, ts, bs) = (v, [ls, rs, ts, bs])

checkVisible :: [[Int]] -> [[Bool]]
checkVisible = map (map check . middle) . middle . lookAround
  where
    check (x, ys) = any ((x >) . maximum) ys

scores :: [[Int]] -> [[Int]]
scores = map (map score) . lookAround

score :: (Int, [[Int]]) -> Int
score (v, xss) = product $ map (viewDistanceFrom v) xss

viewDistanceFrom :: Int -> [Int] -> Int
viewDistanceFrom v = go 0
  where
    go n [] = n
    go n (x:xs)
      | x >= v    = n + 1
      | otherwise = go (n + 1) xs

middle :: [a] -> [a]
middle = init . tail

example :: [[Int]]
example =
  [ [3, 0, 3, 7, 3]
  , [2, 5, 5, 1, 2]
  , [6, 5, 3, 3, 2]
  , [3, 3, 5, 4, 9]
  , [3, 5, 3, 9, 0]
  ]
