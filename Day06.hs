{-# OPTIONS -Wall #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Day06 where

import Data.Set qualified as Set
import Data.List (tails)

main :: IO ()
main = do
  inp <- readFile "Day06.input"
  print $ findMarkWSize 4 inp  -- 1848
  print $ findMarkWSize 14 inp -- 2308

findMarkWSize :: Int -> String -> Int
findMarkWSize n =
  fst . head . filter (allDifferent . snd)
  . zip [n..] . map (take n) . tails

allDifferent :: (Ord a, Eq a) => [a] -> Bool
allDifferent l = length l == Set.size (Set.fromList l)
