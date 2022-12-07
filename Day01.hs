module Main where

import Data.List (sort)

main :: IO ()
main = do
  xs <- decode . lines <$> readFile "Day01.input"
  print $ solution1 xs -- 70720
  print $ solution2 xs -- 207148

decode :: [String] -> [[Int]]
decode = foldr step []
  where
    step "" acc   = [] : acc
    step x []     = [[read x]]
    step x (a:as) = (read x : a) : as

solution1 :: [[Int]] -> Int
solution1 = maximum . map sum

solution2 :: [[Int]] -> Int
solution2 = sum . take 3 . reverse . sort . map sum
