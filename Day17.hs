module Main where

import Data.List (foldl')

import Data.HashMap.Strict qualified as HM

data Move = L | R deriving (Show)
newtype Block = Block [[Bool]] deriving (Show)

type Chamber = [[Bool]]
type Top = [[Bool]]

main :: IO ()
main = do
  solution example
  -- => 3068
  -- => 1514285714288
  xs <- decode <$> readFile "Day17.input"
  solution xs
  -- => 3211
  -- => TODO: 1589142857183 why ???

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

dropBlock
  :: Block
  -> [(Int, Move)]
  -> Int --  ^ current height
  -> Chamber
  -> ( Int --  ^ total height
     , Chamber, [(Int, Move)])
dropBlock block moves heightNow = go [] block moves . (ss <>)
  where
    ss = spacer block
    height = let Block b = block in length b
    go ts b ((_, m):ms) cs@(c:rest) | length cs >= height =
        let mb = move m b
            bb = if fits mb cs then mb else b
        in if length cs == height || not (fits bb $ tail cs)
           then let (e, cs') = crop $ reverse ts <> place bb cs
                in (heightNow + length ss - e, cs', ms)
           else go (c:ts) bb ms rest
    go _ b ms cs = error $ show (b, ms, cs)
    crop xs = let e = length $ takeWhile (not . or) xs
              in (e, take 50 -- an empyrical value
                     $ drop e xs)

place :: Block -> Chamber -> Chamber
place (Block bs) cs =
  zipWith (zipWith (||)) bs (take h cs) <> drop h cs
  where
    h = length bs

spacer :: Block -> Chamber
spacer (Block b) = replicate (3 + length b) $ replicate 7 False

dropSome
  :: Chamber -> [(Int, Block)] -> [(Int, Move)]
  -> ( Int  --  ^ height
     , Chamber  --  ^ Chamber at the end
     , Maybe ( (Int, Int)  --  ^ rocks & height @ the start of the cycle
             , (Int, Int)  --  ^ size & height of the cycle
             , Chamber     --  ^ chamber at the end of the first cycle
             , [(Int, Block)]     --  ^ blocks at ...
             , [(Int, Move)]      --  ^ rest moves at ...
             ))
dropSome chamber blks =
  go 0 blks HM.empty Nothing 0 chamber
  where
    go _ []     _   cy h cs  _ = (h, cs, cy)
    go _ _      _   _  _ _  [] = error "impossible"
    go i bs@((bi, b):_) !hm cy h cs ms@((mi, _) : _) =
      let (h', cs', ms') = dropBlock b ms h cs
          (hm', cy') = case cy of
            Just _  -> (hm, cy)
            Nothing ->
              let k = (bi, mi, chamberKey cs)
              in case HM.lookup k hm of
                Nothing       -> (HM.insert k (i, h) hm, Nothing)
                Just (n0, h0) -> (hm, Just ((n0, h0), (i - n0, h - h0), cs, bs, ms))
      in go (i + 1) (tail bs) hm' cy' h' cs' ms'

chamberKey :: Chamber -> [Int]
chamberKey = map fst . foldl' step (replicate 7 (0, False))
  where
    step = zipWith \(n, a) b ->
      if a then (n, a) else
        if b then (n, b) else
          (n + 1, b)

solution :: [Move] -> IO ()
solution moves = case dropSome [] (take 2022 bs0) ms0 of
  (h, _, Just ((b0, h0), (cb, ch), cs, bs, ms)) -> do
    print h
    print (cb, ch)
    let (n, r) =
          ((1000000000000 :: Integer) - fromIntegral b0)
          `divMod` fromIntegral cb
    let (rh, _, _) = dropSome cs (take (fromIntegral r) bs) ms
    print $ fromIntegral h0 + n * fromIntegral ch + fromIntegral rh
  _ -> error "impossible"
  where
    bs0 = cycle $ zip [0..] blocks
    ms0 = cycle $ zip [0..] moves

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
