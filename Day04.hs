{-# OPTIONS -Wall #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Day04 where

import Data.Char
import Data.Set qualified as Set

import ReadP -- Why not?

type Pair = ((Int, Int), (Int, Int))

main :: IO ()
main = do
  xs <- map decode . lines <$> readFile "Day04.input"
  print $ solution1 xs -- 599
  print $ solution2 xs -- 928

solution1 :: [Pair] -> Int
solution1 = length . filter areFullyContain . map toSets

solution2 :: [Pair] -> Int
solution2 = length . filter areOverlap . map toSets

areFullyContain :: (Set.Set Int, Set.Set Int) -> Bool
areFullyContain (s1, s2) = s1 `Set.isSubsetOf` s2 || s2 `Set.isSubsetOf` s1

areOverlap :: (Set.Set Int, Set.Set Int) -> Bool
areOverlap (s1, s2) = not . Set.null $ s1 `Set.intersection` s2

toSets :: Pair -> (Set.Set Int, Set.Set Int)
toSets ((f1, t1), (f2, t2)) = (s1, s2)
  where
    s1 = Set.fromList [f1 .. t1]
    s2 = Set.fromList [f2 .. t2]

decode :: String -> Pair
decode = tryReadP "line" lineP

lineP :: ReadP Pair
lineP = (,) <$> rangeP <* char ',' <*> rangeP

rangeP :: ReadP (Int, Int)
rangeP = (,) <$> numP <* char '-' <*> numP

numP :: ReadP Int
numP = read <$> munch1 isDigit
