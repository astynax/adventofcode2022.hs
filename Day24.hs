{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}

module Main where

import Data.HashSet qualified as HS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Graph.AStar
import System.Console.ANSI qualified as ANSI

import Pos

data Input = Input
  { iWidth  :: Int
  , iHeight :: Int
  , iRows   :: Map Int (Set Int, Set Int)
  , iCols   :: Map Int (Set Int, Set Int)
  } deriving Show

main :: IO ()
main = do
  -- print $ solution example -- (18,54)
  i <- decode <$> readFile "Day24.input"
  -- animate i
  print $ solution i -- (314,896)

decode :: String -> Input
decode s =
  Input
  { iWidth  = length (head body)
  , iHeight = length body
  , iRows   = Map.fromList $ zip [0..] rows
  , iCols   = Map.fromList $ zip [0..] cols
  }
  where
    body = map (init . tail) . init . tail $ lines s
    rows =
      [ ( toSetOf '<' row, toSetOf '>' $ reverse row)
      | row <- body ]
    cols =
      [ ( toSetOf '^' col, toSetOf 'v' $ reverse col)
      | y <- [0 .. length (head body) - 1]
      , let col = map (!! y) body
      ]
    toSetOf x xs = Set.fromList [ i | (i, c) <- zip [0..] xs, c == x]

solution :: Input -> (Int, Int)
solution i@(Input w h _ _) =
  let start = (0, -1)
      exit = (w - 1, h)
      t1 = run i 0 start exit
      t2 = run i t1 exit start
      t3 = run i t2 start exit
  in (t1, t3)

run :: Input -> Int -> Pos -> Pos -> Int
run i time start exit =
  case reverse <$> search i time start exit of
    Just ((_, x) : _) -> x
    _ -> error "Impossible"

search :: Input -> Int -> Pos -> Pos -> Maybe [(Pos, Int)]
search i@(Input w h _ _) time start exit =
  aStar fork comp dist goal (start, time)
  where
    comp _ _ = 1
    dist (p, _) = manhattan p exit
    goal (p, _) = p == exit
    fork ((x, y), t) = HS.fromList
      [ (n, t + 1)
      | n@(nx, ny) <- (x, y) : neibs (x, y)
      , n == exit || n == start || (
         nx >= 0 && nx < w && ny >= 0 && ny < h
         && isSafeAt i (t + 1) n
      )
      ]

isSafeAt :: Input -> Int -> Pos -> Bool
isSafeAt (Input w h rows cols) t (x, y) =
  let (ls, rs) = rows Map.! y
      (us, ds) = cols Map.! x
  in not
     ( ((t + x)         `mod` w) `Set.member` ls ||
       ((t + w - x - 1) `mod` w) `Set.member` rs ||
       ((t + y)         `mod` h) `Set.member` us ||
       ((t + h - y - 1) `mod` h) `Set.member` ds )

example :: Input
example = decode $ unlines
  [ "#.######"
  , "#>>.<^<#"
  , "#.<..<<#"
  , "#>v.><>#"
  , "#<^v^^>#"
  , "######.#" ]

draw :: Int -> Input -> IO ()
draw t i@(Input w h _ _) =
  mapM_ putStrLn
  [ [ if isSafeAt i t (x, y) then '.' else '#'
    | x <- [0 .. w - 1]
    ]
  | y <- [0 .. h - 1]
  ]

animate :: Input -> IO ()
animate i@(Input w h rows cols) = do
  let Just p = (((0, 0), 1) :) <$> search i 2 (0, 0) (w - 1, h - 1)
  go p
  where
    go [] = pure ()
    go ((pos, t) : path) = do
      ANSI.clearScreen
      mapM_ putStrLn
        [ [ if | (x, y) == pos -> 'E'
               | m > 1 -> head $ show m
               | l -> '<'
               | r -> '>'
               | u -> '^'
               | d -> 'v'
               | otherwise -> '.'
          | x <- [0 .. w - 1]
          , let (ls, rs) = rows Map.! y
          , let (us, ds) = cols Map.! x
          , let l = ((t + x)         `mod` w) `Set.member` ls
          , let r = ((t + w - x - 1) `mod` w) `Set.member` rs
          , let u = ((t + y)         `mod` h) `Set.member` us
          , let d = ((t + h - y - 1) `mod` h) `Set.member` ds
          , let m = length $ filter id [l, r, u, d]
          ]
        | y <- [0 .. h - 1]
        ]
      _ <- getChar
      go path
