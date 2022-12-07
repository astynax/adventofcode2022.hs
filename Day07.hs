module Main where

import Data.Char (isAlphaNum)

import Megaparsec
import Data.List (sort)

data Command
  = Cd Where
  | Ls [Either Dir File]
  deriving (Show)

data Where = Up | Root | To Dir
  deriving Show

newtype Dir = Dir String
  deriving (Show, Eq)

data File = File Int String
  deriving Show

data Tree = Tree ![(Dir, Tree)] ![File] deriving Show

type TreeZipper = (Tree, [TreeZipperLevel])
data TreeZipperLevel = TZL
  { tzDir     :: !Dir
  , tzFiles   :: ![File]
  , tzLefts   :: ![(Dir, Tree)]
  , tzRights  :: ![(Dir, Tree)]
  } deriving (Show)
type Movement = TreeZipper -> TreeZipper

main :: IO ()
main = do
  inp <- sizes . build . tryParse inputP "input" <$> readFile "Day07.input"
  print $ solution1 inp -- 1086293
  print $ solution2 inp -- 366028

-- solutions

solution1 :: (Int, [Int]) -> Int
solution1 = sum. filter (<= 100000) . snd

solution2 :: (Int, [Int]) -> Int
solution2 (total, ss) =
  let delta = total - 40000000
  in case filter (>= delta) (sort ss) of
    (x : _) -> x
    _       -> error "Can't find an answer!"

sizes :: Tree -> (Int, [Int])
sizes (Tree ds fs) = (total, total : concatMap snd dss)
  where
    total = td + tf
    dss = map (sizes . snd) ds
    td = sum $ map fst dss
    tf = sum [s | File s _ <- fs]

-- tree zipper

build :: [Command] -> Tree
build = go (Tree [] [], [])
  where
    go z []     = fst $ cdRoot z
    go z (c:cs) = go (step c z) cs

cdRoot :: Movement
cdRoot z@(_, []) = z
cdRoot z         = cdRoot $ cdUp z

cdUp :: Movement
cdUp (_, []) = error "Can't go up further!"
cdUp (t, (TZL d fs ls rs) : ps) = (tt, ps)
  where
    tt = Tree (reverse ls <> ((d, t) : rs)) fs

cdTo :: Dir -> Movement  -- works like "mkdir && cd"
cdTo d (Tree ds fs, ps) = case break ((== d) . fst) ds of
  (xs, [])          -> (Tree [] [], TZL d fs xs [] : ps)
  (ls, (_, t) : rs) -> (t,          TZL d fs ls rs : ps)

update :: [Either Dir File] -> Movement
update xs (_, ps) =
  (Tree [(d, Tree [] []) | Left d <- xs] [f | Right f <- xs], ps)

step :: Command -> Movement
step (Cd Up)     = cdUp
step (Cd Root)   = cdRoot
step (Cd (To d)) = cdTo d
step (Ls xs)     = update xs

-- parsing

inputP :: Parser [Command]
inputP = many (Cd <$> cdP <* char '\n' <|> Ls <$> lsP) <* eof

cdP :: Parser Where
cdP = string "$ cd " *>
  ( (Root <$ string "/")
    <|> (Up <$ string "..")
    <|> (To . Dir <$> nameP)
  )

lsP :: Parser [Either Dir File]
lsP = string "$ ls\n" *> itemsP
  where
    itemsP = many $ (Left <$> dirP <|> Right <$> fileP) <* char '\n'

dirP :: Parser Dir
dirP = Dir <$> (string "dir " *> nameP)

fileP :: Parser File
fileP = File <$> decimal <* string " " <*> nameP

nameP :: Parser String
nameP = some $ satisfy \c -> isAlphaNum c || c == '.'
