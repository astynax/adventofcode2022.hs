{-# OPTIONS -Wall #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Day05 where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List (foldl', sort)

import ReadP

type Stacks = Map Int [Char]
type Move = (Int, Int, Int)

type Stepper = Stacks -> [Move] -> (Stacks, [Move])

main :: IO ()
main = do
  (start, steps) <- decode . lines <$> readFile "Day05.input"
  print $ solution1 start steps -- "VJSFHWGFT"
  print $ solution2 start steps -- "LCTQFBVZV"

decode :: [String] -> (Stacks, [Move])
decode xs = case break null xs of
  (ss, _ : ms) -> (decodeStacks $ init ss, map decodeMove ms)
  _ -> error "Can't split stack and moves"

decodeMove :: String -> Move
decodeMove =
  tryReadP "move" $ (,,)
  <$> (string "move "  *> intP)
  <*> (string " from " *> intP)
  <*> (string " to "   *> intP)

decodeStacks :: [String] -> Stacks
decodeStacks = build . reverse . map (fromLine 1)
  where
    build = foldl (foldl' put) Map.empty
    put m (i, c) = Map.insertWith (<>) i [c] m

    fromLine :: Int -> String -> [(Int, Char)]
    fromLine _ []             = []
    fromLine n (' ':xs)       = fromLine (n + 1) $ drop 3 xs
    fromLine n ('[':c:']':xs) = (n, c) : fromLine (n + 1) (drop 1 xs)
    fromLine _ xs = error $ "Can't decode this: " <> show xs

solution1 :: Stacks -> [Move] -> String
solution1 s = tops . run step s

solution2 :: Stacks -> [Move] -> String
solution2 s = tops . run step2 s

tops :: Stacks -> String
tops = concatMap (take 1 . snd) . sort . Map.toList

run :: Stepper -> Stacks -> [Move] -> Stacks
run f s ms = case f s ms of
  (s', [])  -> s'
  (s', ms') -> run f s' ms'

step :: Stepper
step s [] = (s, [])
step s ((0, _, _) : ms) = (s, ms)
step s ((n, f, t) : ms) =
  let v = case Map.lookup f s of
        Just (x : _) -> x
        _            -> error $ "Bad move: " <> show (n, f, t)
      s' = Map.update (Just . (v :)) t $ Map.update (Just . tail) f s
  in (s', (n - 1, f, t) : ms)

step2 :: Stepper
step2 s [] = (s, [])
step2 s ((n, f, t) : ms) =
  let v = case take n <$> Map.lookup f s of
        Just x
          | length x == n
            -> x
        _   -> error $ "Bad move: " <> show (n, f, t)
      s' = Map.update (Just . (v <>)) t $ Map.update (Just . drop n) f s
  in (s', ms)
