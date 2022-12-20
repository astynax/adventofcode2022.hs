{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.List

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Megaparsec

newtype Ref = Ref Int deriving (Show, Eq, Ord)
data Link = Link !Int !Ref !Ref deriving Show
type Chain = Map Ref Link

data Input = Input !Int ![Int]

main :: IO ()
main = do
  xs <- decode <$> readFile "Day20.input"
  print $ solution1 xs -- 11616
  print $ solution2 xs -- 9937909178485

decode :: String -> [Int]
decode = tryParse numsP "input"
  where
    numsP :: Parser [Int]
    numsP = many $ integerP <* "\n"

solution1 :: [Int] -> Int
solution1 = run 1

solution2 :: [Int] -> Int
solution2 = run 10 . map (811589153 *)

run :: Int -> [Int] -> Int
run n xs =
  let c = foldl' (flip ($)) (toChain xs) $ replicate n mix
  in sum [v | Link v _ _ <- map (c `at`) [1000, 2000, 3000]]

mix :: Chain -> Chain
mix m = foldl' (flip move) m [Ref i | i <- [0 .. Map.size m - 1]]

move :: Ref -> Chain -> Chain
move r m =
  let Link v rp rn = m Map.! r
      offset = v `toOffsetBelow` (Map.size m - 1)
  in if offset == 0 then m else
       let
         afterRemove =
           relink rp Nothing (Just rn) $
           relink rn (Just rp) Nothing m
         (n, Link _ _ nn) = go afterRemove offset  r
       in relink n Nothing (Just r) $
          relink nn (Just r) Nothing $
          relink r (Just n) (Just nn) afterRemove
  where
    go chain dx x
      | dx < 0    = go chain (dx + 1) p
      | dx > 0    = go chain (dx - 1) n
      | otherwise = (x, l)
      where
        l@(Link _ p n) = chain Map.! x

toOffsetBelow :: Int -> Int -> Int
toOffsetBelow x s
  | x < 0     = negate ax - 1
  | otherwise = ax
  where
    ax = abs x `mod` s

relink :: Ref -> Maybe Ref -> Maybe Ref -> Chain -> Chain
relink k np nn =
  (`Map.update` k) \(Link v p n) ->
  Just (Link v (fromMaybe p np) (fromMaybe n nn))

at :: Chain -> Int -> Link
at m = go (find0 m) . (`mod` Map.size m)
  where
    go r i
      | i == 0    = l
      | otherwise = go n (i - 1)
      where
        l@(Link _ _ n) = m Map.! r

find0 :: Chain -> Ref
find0 m = case filter (\(_, Link v _ _) -> v == 0) (Map.toList m) of
  [(r, _)] -> r
  _        -> error "Impossible"

off :: Ref -> Int -> Chain -> Ref
off (Ref r) n m = Ref ((r + n) `mod` Map.size m)

toChain :: [Int] -> Chain
toChain xs = Map.fromList
  [ (Ref i, Link x (Ref p) (Ref n))
  | (x, i, p, n) <- zip4 xs [0..] (l : [0..]) (take l [1..] <> [0])
  ]
  where
    l = length xs - 1

fromChain :: Chain -> [Int]
fromChain m = go Nothing (find0 m)
  where
    go start pos
      | Just pos == start = []
      | otherwise =
        let Link v _ n = m Map.! pos
        in v : go (start <|> Just pos) n

example :: [Int]
example = [1, 2, -3, 3, -2, 0, 4]

debugMove :: Ref -> Chain -> IO ()
debugMove r m = do
  dump m
  dump $ move r m

dump :: Chain -> IO ()
dump = print . fromChain

check :: IO ()
check = do
  assertEq (fromChain e) [0, 4, 1, 2, -3, 3, -2]
  assertEq (map fromChain $ scanl' (flip move) e [Ref i | i <- [0..6]])
    [ [0, 4, 1, 2, -3, 3, -2]
    , [0, 4, 2, 1, -3, 3, -2]
    , [0, 4, 1, -3, 2, 3, -2]
    , [0, 4, 1, 2, 3, -2, -3]
    , [0, 3, 4, 1, 2, -2, -3]
    , [0, 3, 4, -2, 1, 2, -3]
    , [0, 3, 4, -2, 1, 2, -3]
    , [0, 3, -2, 1, 2, -3, 4]
    ]
  assertEq
    [ v | let c = toChain [0, 3, -2, 1, 2, -3, 4]
        , Link v _ _ <- map (c `at`) [1000, 2000, 3000] ]
    [4, -3, 2]
  where
    e = toChain example
    assertEq a b
      | a == b = pure ()
      | otherwise = error $ show a <> " /= " <> show b
