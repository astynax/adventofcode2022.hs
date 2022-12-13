{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sortBy, elemIndex)
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))

import Megaparsec

type Input = [(Packet, Packet)]
newtype Packet = Packet [Either Int Packet] deriving (Eq, Show)

instance IsString Packet where
  fromString = tryParse packetP "packet"

main :: IO ()
main = do
  inp <- tryParse inputP "input" <$> readFile "Day13.input"
  print $ solution1 inp -- 4643
  print $ solution2 inp -- 21614

inputP :: Parser Input
inputP = pairP `sepBy` "\n"
  where
    pairP = (,) <$> packetP <* "\n" <*> packetP <* "\n"

packetP :: Parser Packet
packetP = between "[" "]" $ Packet <$> itemP `sepBy` ","
  where
    itemP = Left <$> decimal <|> Right <$> packetP

inOrder :: Packet -> Packet -> Bool
inOrder (Packet p1) (Packet p2) =
  fromMaybe (error $ "Undecidable: " <> show p1 <> " vs " <> show p2)
  $ test p1 p2
  where
    test (Right (Packet l) : ls) (Right (Packet r) : rs) =
      case (l, r) of
        ([], []) -> test ls rs
        _        -> test l r <|> test ls rs
    test (Left l : ls) (Left r : rs)
      | l == r    = test ls rs
      | otherwise = Just $ l < r
    test (l@(Left _) : ls) r = test (Right (Packet [l]) : ls) r
    test l (r@(Left _) : rs) = test l (Right (Packet [r]) : rs)
    test [] (_ : _) = Just True
    test (_ : _) [] = Just False
    test _ _ = Nothing

solution1 :: Input -> Int
solution1 = sum . map fst . filter snd . zip [1..] . map (uncurry inOrder)

solution2 :: Input -> Int
solution2 =
  fromMaybe (error "Undecidable!") . findDividers
  . sortBy cmp . (<> [d2, d6]) . concatMap \(p1, p2) -> [p1, p2]
  where
    findDividers l = (*) <$> idxOf d2 <*> idxOf d6
      where
        idxOf v = (1 +) <$> elemIndex v l
    cmp p1 p2
      | inOrder p1 p2 = EQ
      | otherwise = GT
    d2 = "[[2]]"
    d6 = "[[6]]"

example :: String
example = unlines
  [ "[1,1,3,1,1]"
  , "[1,1,5,1,1]"
  , ""
  , "[[1],[2,3,4]]"
  , "[[1],4]"
  , ""
  , "[9]"
  , "[[8,7,6]]"
  , ""
  , "[[4,4],4,4]"
  , "[[4,4],4,4,4]"
  , ""
  , "[7,7,7,7]"
  , "[7,7,7]"
  , ""
  , "[]"
  , "[3]"
  , ""
  , "[[[]]]"
  , "[[]]"
  , ""
  , "[1,[2,[3,[4,[5,6,7]]]],8,9]"
  , "[1,[2,[3,[4,[5,6,0]]]],8,9]"
  ]
