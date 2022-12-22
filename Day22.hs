{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Megaparsec
import Map2d
import Pos

data Side = Side !Int !Int deriving (Show, Ord, Eq)
type BoardMap = Map Side (Map2d Bool)
type Path = Rope Int Turn
data Rope a b = Rope
  { rHead :: !a
  , rTail :: Maybe (Rope b a)
  } deriving Show

data Input = Input
  { iBoard :: Board
  , iPath  :: Path
  , iSide  :: Side
  , iPos   :: Pos
  } deriving (Show)

type Teleporter = Board -> Cursor -> Cursor

data Board = Board
  { sSideWidth  :: Int
  , sSideHeight :: Int
  , sBoard      :: BoardMap
  } deriving (Show)

data Cursor = Cursor !Side !Pos !Direction deriving (Show)

main :: IO ()
main = do
  print $ solution simple      example -- 6032
  print $ solution exampleTele example -- 5031
  input <- decode 50 50 <$> readFile "Day22.input"
  print $ solution simple    input -- 26558
  print $ solution inputTele input -- 110400

decode :: Int -> Int -> String -> Input
decode sw sh input =
  Input
  { iBoard = Board sw sh m
  , iPath  = decodePath ps
  , iSide  = Side (hx `div` sw) 0
  , iPos   = (hx `mod` sw, 0)
  }
  where
    (bs, [_, ps]) = break (== "") $ lines input
    hx = length . takeWhile (== ' ') $ head bs
    w = maximum (map length bs) `div` sw
    h = length bs `div` sh
    fromChar = const \case
      '#' -> Just False
      '.' -> Just True
      _   -> Nothing
    m = Map.fromList
      [ (Side sx sy, sm)
      | sx <- [0 .. w - 1]
      , sy <- [0 .. h - 1]
      , let block = map (slice sx sw) $ slice sy sw bs
      , let sm = Map2d.build fromChar block
      , not $ Map.null sm
      ]
    slice from by = take by . drop (from * by)

decodePath :: String -> Path
decodePath = tryParse pathP "path"
  where
    pathP :: Parser Path
    pathP = ropeP decimal dirP
    dirP = TL <$ "L" <|> TR <$ "R"

ropeP :: Parser a -> Parser b -> Parser (Rope a b)
ropeP pa pb = Rope <$> pa <*> optional (ropeP pb pa)

forward
  :: Cursor -> Path
  -> Reader (Teleporter, Board) Cursor
forward cur@(Cursor side pos dir) (Rope n r)
  | n == 0    = tryToTurn
  | otherwise = ask >>= \(tele, brd@(Board _ _ bm)) ->
    let pos' = Pos.moveTo dir pos
        (newCur, walkable) =
          case Map.lookup pos' (bm Map.! side) of
            Nothing ->
              let c@(Cursor s p _) = tele brd cur
              in (c, bm Map.! s Map.! p)
            Just w  -> (Cursor side pos' dir, w)
    in if walkable
         then forward newCur (Rope (n - 1) r)
         else tryToTurn
  where
    tryToTurn = maybe (pure cur) (turnHead cur) r

turnHead
  :: Cursor -> Rope Turn Int
  -> Reader (Teleporter, Board) Cursor
turnHead (Cursor side pos dir) (Rope t r) =
  let dir' = Pos.turn t dir
      cur' = Cursor side pos dir'
  in maybe (pure cur') (forward cur') r

simple :: Teleporter
simple (Board sw sh bm) (Cursor s@(Side sx sy) (x, y) dir) =
  case Map.lookup s' bm of
    Nothing ->
      case dir of
        U -> Cursor (Side sx $ maximum ys) (x, sh - 1) dir
        D -> Cursor (Side sx $ minimum ys) (x,      0) dir
        L -> Cursor (Side (maximum xs) sy) (sw - 1, y) dir
        R -> Cursor (Side (minimum xs) sy) (0,      y) dir
    Just _ ->
      case dir of
        U -> Cursor s' (x, sh - 1) dir
        D -> Cursor s' (x,      0) dir
        L -> Cursor s' (sw - 1, y) dir
        R -> Cursor s' (0,      y) dir
  where
    s' = sideAt s dir
    xs = [ xx | (Side xx yy, _) <- Map.toList bm, yy == sy ]
    ys = [ yy | (Side xx yy, _) <- Map.toList bm, xx == sx ]

sideAt :: Side -> Direction -> Side
sideAt (Side sx sy) dir = uncurry Side $ moveTo dir (sx, sy)

solution :: Teleporter -> Input -> Int
solution tele inp =
  let
    Board sw sh _ = iBoard inp
    Cursor (Side sx sy) (x, y) dir =
      forward (Cursor (iSide inp) (iPos inp) R) (iPath inp)
      `runReader` (tele, iBoard inp)
    xx = sx * sw + x
    yy = sy * sh + y
  in 1000 * (yy + 1) + 4 * (xx + 1) + case dir of
    R -> 0
    D -> 1
    L -> 2
    U -> 3

--    [1][2]
--    [3]
-- [4][5]
-- [6]
inputTele :: Teleporter
inputTele (Board sw sh _) (Cursor s (x, y) dir) =
  case s of
    _ | s == s1 -> case dir of
      U -> Cursor s6 (0, x) R
      D -> Cursor s3 (x, 0) D
      L -> Cursor s4 (0, invY) R
      R -> Cursor s2 (0, y) R
      | s == s2 -> case dir of
      U -> Cursor s6 (x, h) U
      D -> Cursor s3 (w, x) L
      L -> Cursor s1 (w, y) L
      R -> Cursor s5 (w, invY) L
      | s == s3 -> case dir of
      U -> Cursor s1 (x, h) U
      D -> Cursor s5 (x, 0) D
      L -> Cursor s4 (y, 0) D
      R -> Cursor s2 (y, h) U
      | s == s4 -> case dir of
      U -> Cursor s3 (0, x) R
      D -> Cursor s6 (x, 0) D
      L -> Cursor s1 (0, invY) R
      R -> Cursor s5 (0, y) R
      | s == s5 -> case dir of
      U -> Cursor s3 (x, h) U
      D -> Cursor s6 (w, x) L
      L -> Cursor s4 (w, y) L
      R -> Cursor s2 (w, invY) L
      | s == s6 -> case dir of
      U -> Cursor s4 (x, h) U
      D -> Cursor s2 (x, 0) D
      L -> Cursor s1 (y, 0) D
      R -> Cursor s5 (y, h) U
    _ -> error $ "Impossible: " <> show (s, dir)
  where
    s1 = Side 1 0
    s2 = Side 2 0
    s3 = Side 1 1
    s4 = Side 0 2
    s5 = Side 1 2
    s6 = Side 0 3
    invY = sh - 1 - y
    w = sw - 1
    h = sh - 1

exampleTele :: Teleporter
exampleTele (Board sw sh _) (Cursor s (x, y) dir) =
  case (s, dir) of
    (Side 2 0, U) -> Cursor (Side 0 1) (invX, 0) D
    (Side 2 0, D) -> Cursor (Side 2 1) (x, 0) D
    (Side 2 0, L) -> Cursor (Side 1 1) (y, 0) D
    (Side 2 0, R) -> Cursor (Side 3 2) (w, invY) L
    (Side 0 1, U) -> Cursor (Side 2 0) (invX, 0) D
    (Side 0 1, D) -> Cursor (Side 2 2) (invX, h) U
    (Side 0 1, L) -> Cursor (Side 3 2) (invY, h) U
    (Side 0 1, R) -> Cursor (Side 1 1) (0, y) R
    (Side 1 1, U) -> Cursor (Side 2 0) (0, x) R
    (Side 1 1, D) -> Cursor (Side 2 2) (0, invX) R
    (Side 1 1, L) -> Cursor (Side 0 1) (w, y) L
    (Side 1 1, R) -> Cursor (Side 2 1) (0, y) R
    (Side 2 1, U) -> Cursor (Side 2 0) (x, h) U
    (Side 2 1, D) -> Cursor (Side 2 2) (x, 0) D
    (Side 2 1, L) -> Cursor (Side 1 1) (w, y) L
    (Side 2 1, R) -> Cursor (Side 3 2) (invY, 0) D
    (Side 2 2, U) -> Cursor (Side 2 1) (x, h) U
    (Side 2 2, D) -> Cursor (Side 0 1) (invX, h) U
    (Side 2 2, L) -> Cursor (Side 1 1) (invY, h) U
    (Side 2 2, R) -> Cursor (Side 3 2) (0, y) R
    (Side 3 2, U) -> Cursor (Side 2 1) (w, invX) L
    (Side 3 2, D) -> Cursor (Side 0 1) (0, invX) R
    (Side 3 2, L) -> Cursor (Side 2 2) (w, y) L
    (Side 3 2, R) -> Cursor (Side 2 0) (w, invY) L
    _ -> error $ "Impossible: " <> show (s, dir)
  where
    invX = sw - 1 - x
    invY = sh - 1 - y
    w = sw - 1
    h = sh - 1

example :: Input
example = decode 4 4 $ unlines
  [ "        ...#"  --       [1]
  , "        .#.."  -- [2][3][4]
  , "        #..."  --       [5][6]
  , "        ...."
  , "...#.......#"
  , "........#..."
  , "..#....#...."
  , "..........#."
  , "        ...#...."
  , "        .....#.."
  , "        .#......"
  , "        ......#."
  , ""
  , "10R5L5R10L4R5L5"
  ]
