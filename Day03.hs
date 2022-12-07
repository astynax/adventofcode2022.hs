module Main where

import Data.Char (ord)
import Data.Set qualified as Set

main :: IO ()
main = do
  rucksacks <- lines <$> readFile "Day03.input"
  print $ solution1 rucksacks -- 8202
  print $ solution2 rucksacks -- 2864

solution1 :: [String] -> Int
solution1 =
  sum . map (sum . map priority . Set.toList . match . toRucksack)
  where
    match (s1, s2) = Set.intersection (Set.fromList s1) (Set.fromList s2)

solution2 :: [String] -> Int
solution2 = sum . map (priority . commonInTriple) . triples

triples :: [a] -> [[a]]
triples xs = case splitAt 3 xs of
  (x, []) -> [x]
  (x, ys) -> x : triples ys

commonInTriple :: [String] -> Char
commonInTriple [a, b, c] =
  let s =
        Set.fromList a `Set.intersection`
        Set.fromList b `Set.intersection`
        Set.fromList c
  in case Set.toList s of
    [x] -> x
    i -> error $ "Too big intersection: " <> show i
commonInTriple t = error $ "Too big triple: " <> show t

toRucksack :: String -> (String, String)
toRucksack s = splitAt (length s `div` 2) s

priority :: Char -> Int
priority c = ord c - if c <= 'Z' then 38 else 96
