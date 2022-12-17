module Main where

data Move = L | R deriving (Show)

newtype Block = Block [[Bool]] deriving Show
type Chamber = [[Bool]]

main :: IO ()
main = do
  print $ solution1 example -- 3068
  xs <- decode <$> readFile "Day17.input"
  pure ()

decode :: String -> [Move]
decode = map toMove . takeWhile \c -> c == '>' || c == '<'
  where
    toMove '<' = L
    toMove _   = R

move :: Move -> Block -> Block
move m b@(Block rs) = case m of
  L | any head rs -> b
    | otherwise   -> Block $ map ((<> [False]) . tail) rs
  R | any last rs -> b
    | otherwise   -> Block $ map ((False :) . init) rs

fits :: Block -> Chamber -> Bool
fits (Block bs) cs =
  not . or $ zipWith ((or .) . zipWith (&&)) bs $ take (length bs) cs

dropBlock :: Block -> [Move] -> Chamber -> (Chamber, [Move])
dropBlock block moves = go [] block moves . (spacer block <>)
  where
    height = let Block b = block in length b
    go ts b (m:ms) cs@(c:rest) | length cs >= height =
        let mb = move m b
            bb = if fits mb cs then mb else b
        in if length cs == height || not (fits bb $ tail cs)
           then (crop $ reverse ts <> place bb cs, ms)
           else go (c:ts) bb ms rest
    go _ b ms cs = error $ show (b, ms, cs)
    crop = dropWhile (not . or)

place :: Block -> Chamber -> Chamber
place (Block bs) cs =
  zipWith (zipWith (||)) bs (take h cs) <> drop h cs
  where
    h = length bs

spacer :: Block -> Chamber
spacer (Block b) = replicate (3 + length b) $ replicate 7 False

dropSome :: Int -> [Move] -> Chamber
dropSome n = go (take n $ cycle blocks) [] . cycle
  where
    go [] cs _  = cs
    go _  cs [] = cs
    go (b:bs) cs ms = uncurry (go bs) $ dropBlock b ms cs

solution1 :: [Move] -> Int
solution1 = length . dropSome 2022

blocks :: [Block]
blocks = map (Block . toBlock)
  [ [ "..####." ]

  , [ "...#..."
    , "..###.."
    , "...#..." ]

  , [ "....#.."
    , "....#.."
    , "..###.." ]

  , [ "..#...."
    , "..#...."
    , "..#...."
    , "..#...." ]

  , [ "..##..."
    , "..##..." ] ]
  where
    toBlock = map $ map (== '#')

example :: [Move]
example = decode ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

draw :: Chamber -> IO ()
draw = mapM_ row
  where
    row = putStrLn . map \f -> if f then '#' else '.'
