module Day02 where

type Round = (RSP, (RSP, Game))
data Game = Won | Lose | Draw deriving (Eq)
data RSP = R | S | P deriving (Eq, Show)

main :: IO ()
main = do
  games <- map decode . lines <$> readFile "Day02.input"
  print $ solution1 games -- 14297
  print $ solution2 games -- 10498

solution1 :: [Round] -> Int
solution1 = sum . map roundScore1

solution2 :: [Round] -> Int
solution2 = sum . map roundScore2

decode :: String -> Round
decode (o:' ':[y]) = (oo, yy)
  where
    oo = case o of
      'A' -> R
      'B' -> P
      'C' -> S
      _   -> error $ "Bad char: " <> [o]
    yy = case y of
      'X' -> (R, Lose)
      'Y' -> (P, Draw)
      'Z' -> (S, Won)
      _   -> error $ "Bad char: " <> [y]
decode i = error $ "Bad input: " <> i

score :: RSP -> Int
score R = 1
score P = 2
score S = 3

gameScore :: Game -> Int
gameScore Won  = 6
gameScore Draw = 3
gameScore _    = 0

playRound :: RSP -> RSP -> Game
playRound P S = Won
playRound S R = Won
playRound R P = Won
playRound o y | o == y = Draw
playRound _ _ = Lose

roundScore1 :: Round -> Int
roundScore1 (o, (y, _)) = score y + gameScore (playRound o y)

roundScore2 :: Round -> Int
roundScore2 (o, (_, g)) = score y + gameScore g
  where
    y = head [ yy | yy <- [R, S, P], playRound o yy == g ]
