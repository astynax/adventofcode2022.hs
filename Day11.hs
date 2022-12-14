{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Megaparsec

newtype Item = Item Int deriving Show
newtype Idx = Idx Int deriving (Show, Eq, Ord)

type Monkeys = Map Idx Monkey
data Monkey = Monkey
  { mItems           :: [Item]
  , mOperation       :: Operation
  , mTestDivisibleBy :: Int
  , mTestTrue        :: Idx
  , mTestFalse       :: Idx
  } deriving Show

data Operation = Mul Arg | Add Arg deriving Show

data Arg = Int Int | Old deriving Show

main :: IO ()
main = do
  monkeys <- tryParse inputP "input" <$> readFile "Day11.input"
  print $ solution1 monkeys -- 69918
  print $ solution2 monkeys -- 19573408701

inputP :: Parser Monkeys
inputP = Map.fromList <$> (`sepBy` string "\n") do
  k <- "Monkey " *> idxP <* ":\n"
  m <- Monkey
    <$> line "  Starting items: "             itemsP
    <*> line "  Operation: new = old "        opP
    <*> line "  Test: divisible by "          decimal
    <*> line "    If true: throw to monkey "  idxP
    <*> line "    If false: throw to monkey " idxP
  pure (k, m)
  where
    idxP = Idx <$> decimal
    itemsP = (Item <$> decimal) `sepBy` string ", "
    line :: String -> Parser a -> Parser a
    line s p = string s *> p <* string "\n"

opP :: Parser Operation
opP = Add <$> ("+ " *> argP) <|> Mul <$> ("* " *> argP)
  where
    argP = Int <$> decimal <|> Old <$ "old"

throw :: (Item -> Item) -> Monkey -> (Monkey, [(Idx, Item)])
throw limit m@Monkey {..}
  | null mItems = (m, [])
  | otherwise   = (m { mItems = [] }, map check mItems)
  where
    check (limit . applyOp mOperation -> i@(Item v))
      | v `mod` mTestDivisibleBy == 0 = (mTestTrue, i)
      | otherwise                     = (mTestFalse, i)

catch :: Monkeys -> [(Idx, Item)] -> Monkeys
catch = foldl' . flip $ \(k, i) ->
  Map.update (\m -> Just $ m { mItems = mItems m <> [i] }) k

foldRound
  :: (Item -> Item)
  -> (a -> Idx -> Monkey -> Monkey -> a)
  -> a -> Monkeys -> (Monkeys, a)
foldRound limit populate start monkeys =
  foldl' turn (monkeys, start) $ Map.keys monkeys
  where
    turn (ms, s) i = case Map.lookup i ms of
      Nothing -> error "impossible"
      Just m  ->
        let (m', cs) = throw limit m
        in (Map.insert i m' $ catch ms cs, populate s i m m')

foldRounds
  :: Int
  -> (Item -> Item)
  -> (a -> Idx -> Monkey -> Monkey -> a)
  -> (a -> Monkeys -> a)
  -> a -> Monkeys -> a
foldRounds n limit populateMonkey populateRound start monkeys =
  snd $ foldl' step (monkeys, start) [1..n]
  where
    step (!x, !y) _ =
      let (x', s') = foldRound limit populateMonkey y x
      in (x', populateRound s' x')

solution1 :: Monkeys -> Int
solution1 = calculateResult . countInspections 20 divBy3

solution2 :: Monkeys -> Int
solution2 ms = calculateResult $ countInspections 10000 limit ms
  where
    multiplier = foldl1' lcm [mTestDivisibleBy m | (_, m) <- Map.toList ms]
    limit (Item i) = Item $ i `mod` multiplier

calculateResult :: Map Idx Int -> Int
calculateResult = product . take 2 . reverse . sort . map snd . Map.toList

countInspections :: Int -> (Item -> Item) -> Monkeys -> Map Idx Int
countInspections n f = foldRounds n f step const Map.empty
  where
    step !acc i m _ = Map.insertWith (+) i (length $ mItems m) acc

debugRounds :: Int -> Monkeys -> IO ()
debugRounds n =
  mapM_ ((>> putStrLn "") . row) . reverse
  . foldRounds n divBy3 (\x _ _ _ -> x) step []
  where
    row = mapM_ \(i, vs) -> putStrLn $ show i <> ": " <> show vs
    step acc ms =
      [(i, [x | Item x <- mItems m]) | (Idx i, m) <- Map.toList ms] : acc

divBy3 :: Item -> Item
divBy3 (Item i) = Item . floor @Double $ fromIntegral i / 3

applyOp :: Operation -> Item -> Item
applyOp (Add Old)     (Item x) = Item $ x + x
applyOp (Add (Int y)) (Item x) = Item $ x + y
applyOp (Mul Old)     (Item x) = Item $ x * x
applyOp (Mul (Int y)) (Item x) = Item $ x * y
