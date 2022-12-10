{-# LANGUAGE BangPatterns #-}

module Main where

type Op = Maybe Int

main :: IO ()
main = do
  example <- decode . lines <$> readFile "Day10.example"
  print $ solution1 example -- 13140
  draw 40 6 $ mapPixels $ simulate example

  input <- decode . lines <$> readFile "Day10.input"
  print $ solution1 input -- 12880
  draw 40 6 $ mapPixels $ simulate input

solution1 :: [Op] -> Int
solution1 =
  sum . zipWith (*) [20, 60 ..] . take 6
  . every 40 . drop 19
  . simulate

mapPixels :: [Int] -> [Bool]
mapPixels = zipWith check (cycle [0 .. 39])
  where
    check time x = time >= x - 1 && time <= x + 1

decode :: [String] -> [Op]
decode = concatMap decodeLine
  where
    decodeLine "noop" = [Nothing]
    decodeLine s      = [Nothing, Just . read $ drop 5 s]

simulate :: [Op] -> [Int]
simulate = step 1 . cycle
  where
    step !x (o:os) = case o of
      Nothing -> x : step x os
      Just v  -> x : step (x + v) os
    step _ [] = error "impossible"

every :: Int -> [a] -> [a]
every n = map snd . filter ((== 1) . fst) . zip (cycle [1 .. n])

draw :: Int -> Int -> [Bool] -> IO ()
draw w h input =
  mapM_ ((>> putChar '\n') . row)
  [ take w $ drop (w * y) input
  | y <- [0 .. h - 1] ]
  where
    row = mapM_ \x -> putChar $ if x then '#' else '.'
