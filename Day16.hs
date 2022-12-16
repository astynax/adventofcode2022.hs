{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isAlpha)
import Data.List (foldl', sortOn)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Megaparsec

type Cave = Map Name (Rate, [Name])
newtype Name = Name String deriving (Show, Eq, Ord)
newtype Rate = Rate Int deriving (Show, Eq, Ord)

main :: IO ()
main = do
  let e = decode example
  print $ solution1 e -- 1651
  print $ solution2 e -- 1707
  cave <- decode <$> readFile "Day16.input"
  print $ solution1 cave -- 1488
  print $ solution2 cave -- 2111

decode :: String -> Cave
decode = Map.fromList . tryParse inputP "input"

inputP :: Parser [(Name, (Rate, [Name]))]
inputP = many (valveP <* "\n") <* eof
  where
    valveP = (,) <$> ("Valve " *> nameP) <*> (
      (,) <$> (" has flow rate=" *> (Rate <$> decimal))
      <*> (("; tunnels lead to valves " <|>
            "; tunnel leads to valve "
           ) *> (nameP `sepBy` ", ")))
    nameP = Name <$> some (satisfy isAlpha)

example :: String
example = unlines
  [ "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
  , "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
  , "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
  , "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
  , "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
  , "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
  , "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
  , "Valve HH has flow rate=22; tunnel leads to valve GG"
  , "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
  , "Valve JJ has flow rate=21; tunnel leads to valve II"
  ]

solution1 :: Cave -> Int
solution1 cave = maximum $ go [(30, 0, Name "AA", closed)]
  where
    closed = Set.fromList [v | (v, (r, _)) <- Map.toList cave, r > Rate 0]
    go ss
      | null ss   = []
      | otherwise =
        let (rs, ss') = foldl' step ([], []) ss
        in rs <> go (take 1000 $ sortOn (\(_, x, _, _) -> negate x) ss')
    step (rs, ss) (!t, !o, pos, vs)
      | t == 0 || Set.null vs = (o : rs, ss)
      | otherwise =
        let (Rate r, ns) = cave Map.! pos
            ts  = [ (t - 1, o + r * (t - 1), pos, Set.delete pos vs)
                  | pos `Set.member` vs ]
            ss' = [ (t - 1, o, n, vs)
                  | n <- ns
                  ]
        in (rs, ss <> ts <> ss')

solution2 :: Cave -> Int
solution2 cave = maximum $ go [(26, 0, Name "AA", Name "AA", closed)]
  where
    closed = Set.fromList [v | (v, (r, _)) <- Map.toList cave, r > Rate 0]
    go ss
      | null ss   = []
      | otherwise =
        let (rs, ss') = foldl' step ([], []) ss
        in rs <> go (take 3000 $ sortOn (\(_, x, _, _, _) -> negate x) ss')
    step (rs, ss) (!t, !o, p1, p2, vs)
      | t == 0 || Set.null vs = (o : rs, ss)
      | otherwise =
        let (Rate r1, ns1) = cave Map.! p1
            (Rate r2, ns2) = cave Map.! p2
            t1  = [ (t - 1, o + r1 * (t - 1), p1, np, Set.delete p1 vs)
                  | p1 `Set.member` vs
                  , np <- ns2 ]
            t2  = [ (t - 1, o + r2 * (t - 1), np, p2, Set.delete p2 vs)
                  | p2 `Set.member` vs
                  , np <- ns1 ]
            tb  = [ (t - 1, o + r1 * (t - 1) + r2 * (t - 1), p1, p2
                    , Set.delete p2 $ Set.delete p1 vs)
                  | p1 `Set.member` vs && p2 `Set.member` vs
                  ]
            ss' = [ (t - 1, o, n1, n2, vs)
                  | n1 <- ns1
                  , n2 <- ns2
                  , n1 /= n2
                  ]
        in (rs, ss <> t1 <> t2 <> tb <> ss')
