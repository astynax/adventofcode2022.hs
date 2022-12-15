{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Data.List (sort)

import Data.Set (fromList)

import Megaparsec
import Pos

type Reading = (Pos, Pos)
type Sensor = (Pos, Pos, Int)

main :: IO ()
main = do
  ss <- decode <$> readFile "Day15.input"
  print $ solution1 ss -- 5166077
  print $ solution2 ss -- 13071206703981

decode :: String -> [Sensor]
decode = map toSensor . tryParse inputP "input"

inputP :: Parser [Reading]
inputP = many $ readingP <* "\n"
  where
    readingP =
      (,) <$> ("Sensor at " *> posP)
      <*> (": closest beacon is at " *> posP)
    posP = (,) <$> ("x=" *> integerP) <*> (", y=" *> integerP)

toSensor :: Reading -> Sensor
toSensor (s, b) = (s, b, manhattan s b)

solution1 :: [Sensor] -> Int
solution1 = calculate . coverageAtY 2000000
  where
    calculate xs = sum [t - f | (f, t) <- xs]

solution2 :: [Sensor] -> Int
solution2 ss = case filter ((> 1) . length . snd) scans of
  ((y, [(_, x), _]) : _) -> 4000000 * (x + 1) + y
  _ -> error "Impossible!"
  where
    scans = [(y, coverageAtY y ss) | y <- [0 .. 4000000]]

coverageAtY :: Int -> [Sensor] -> [(Int, Int)]
coverageAtY = (merge .) . mapMaybe . intervalAtY

intervalAtY :: Int -> Sensor -> Maybe (Int, Int)
intervalAtY y ((sx, sy), _, d)
  | dy > d    = Nothing
  | otherwise = Just (sx - d + dy, sx + d - dy)
  where
    dy = abs (sy - y)

merge :: [(Int, Int)] -> [(Int, Int)]
merge = go . sort
  where
    go [] = []
    go (x0@(f0, t0) : xs) =
      let nx = (f0, mt)
      in if nx == x0
         then x0 : go rest
         else go (nx : rest)
      where
        mt = maximum $ t0 : [ t | (f, t) <- xs, f <= t0 ]
        rest = [ i | i@(f, _) <- xs, f > t0 ]

draw :: [Sensor] -> IO ()
draw ss =
  drawSetOf toChar . fromList
  $ concat [[s, b] | (s, b, _) <- ss ]
  where
    toChar p occupied
      | occupied && or [ s == p | (s, _, _) <- ss ] = 'S'
      | occupied = 'B'
      | or [ manhattan s p <= d | (s, _, d) <- ss ] = '#'
      | otherwise = '.'
