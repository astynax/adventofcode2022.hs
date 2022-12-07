module Main where

main :: IO ()
main = do
  xs <- lines <$> readFile "DayDAY.input"
  pure ()
