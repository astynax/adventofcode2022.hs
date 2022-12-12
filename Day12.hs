{-# LANGUAGE PackageImports #-}

module Main where

import Data.Char (toUpper)
import Data.Maybe (mapMaybe)

import "astar" Data.Graph.AStar
import Data.HashSet qualified as HS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Pos
import Map2d qualified

type Input = (Pos, Pos, Map Pos Char)

main :: IO ()
main = do
  Just inp <- decode <$> readFile "Day12.input"
  visualize inp
  print $ solution1 inp
  print $ solution2 inp

decode :: String -> Maybe Input
decode s =
  let m = Map2d.build id s
  in do
    start <- Map2d.whereIs 'S' m
    end <- Map2d.whereIs 'E' m
    pure (start, end, Map.insert start 'a' $ Map.insert end 'z' m)

solution1 :: Input -> Maybe Int
solution1 = fmap length . search

solution2 :: Input -> Int
solution2 (_, end, m) = minimum $ mapMaybe (solution1 . (,end,m)) starts
  where
    starts = [ p | (p, c) <- Map.toList m
                 , c == 'a'
                 , or [ n == 'b' | (_, n) <- Map2d.neibs p m ]
                 ]

search :: Input -> Maybe [Pos]
search (start, end, m) =
  aStar fork (\_ _ -> 1) (levi end) (== end) start
  where
    fork pos =
      case Map.lookup pos m of
        Nothing  -> error "impossible"
        Just now -> HS.fromList
          [ p | (p, next) <- Map2d.neibs pos m
              , next <= succ now
              ]

visualize :: Input -> IO ()
visualize inp@(_, _, m) = case search inp of
  Nothing   -> error "impossible"
  Just path ->
    let ps = HS.fromList path
    in Map2d.visualize
       (Map.mapWithKey
        (\k v -> if HS.member k ps then toUpper v else v) m) []

example :: String
example = unlines
  [ "Sabqponm"
  , "abcryxxl"
  , "accszExk"
  , "acctuvwj"
  , "abdefghi"
  ]
