{-# LANGUAGE FlexibleInstances #-}

module Map2d
  ( Map2d
  , build
  , whereIs
  , neibs
  , visualize
  ) where

import Data.List
import Data.Maybe (mapMaybe, fromMaybe)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Pos (Pos)
import Pos qualified

type Map2d = Map Pos

class ToMap2d t where
  build :: (Pos -> Char -> Maybe a) -> t -> Map2d a

instance ToMap2d String where
  build f = build f . lines

instance ToMap2d [String] where
  build f = foldl' buildRow Map.empty . zip [0..]
    where
      buildRow m (y, row) = foldl' (step y) m $ zip [0..] row
      step y m (x, c) = maybe m (Map.insert (x, y) `flip` m) $ f (x, y) c

whereIs :: Eq a => a -> Map2d a -> Maybe Pos
whereIs v m = case filter ((== v) . snd) (Map.toList m) of
  [(pos, _)] -> Just pos
  _          -> Nothing

neibs :: Pos -> Map2d a -> [(Pos, a)]
neibs pos m = mapMaybe check $ Pos.neibs pos
  where
    check k = (k,) <$> Map.lookup k m

visualize :: Char -> Map2d Char -> [Pos] -> IO ()
visualize empty m path =
  draw empty $ foldl' update m $ zip path (tail path)
  where
    update acc (old, new) = Map.insert old d acc
      where
        d = case Pos.whichDirection old new of
          Just Pos.U  -> '^'
          Just Pos.D  -> 'v'
          Just Pos.L  -> '<'
          Just Pos.R  -> '>'
          Nothing ->
            error $ "Impossible jump from "
            <> show old <> " to " <> show new

draw :: Char -> Map2d Char -> IO ()
draw empty m = mapM_ row [minimum ys .. maximum ys]
  where
    row y = mapM_ (cell y) [minimum xs .. maximum xs] >> putChar '\n'
    cell y x = putChar . fromMaybe empty $ Map.lookup (x, y) m
    ps = map fst $ Map.toList m
    xs = map fst ps
    ys = map snd ps
