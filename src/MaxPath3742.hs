module MaxPath3742
  (maxPath) where

import Data.Array
import Data.Function
import Data.List

type Row = Int
type Column = Int
type Cost = Int
type Coord = (Row, Column)
type Grid = Array Coord Cost

pathScores :: [Cost] -> (Int, Int)
pathScores = foldl' updateFn (0, 0)
  where
    updateFn (score, cost) 2 = (score + 2, cost + 1)
    updateFn (score, cost) 1 = (score + 1, cost + 1)
    updateFn (score, cost) _ = (score, cost)

maxPath :: [[Int]] -> Int -> Int
maxPath rawGrid maxCost = if not (null p) then maximum p else -1
  where
    grid = parse rawGrid
    p = allPaths grid
      & map (map (grid !))
      & map pathScores
      & filter (\(_, cost) -> cost <= maxCost)
      & map fst

parse :: [[Int]] -> Grid
parse rawGrid = listArray ((0, 0), (lastRow, lastCol)) (concat rawGrid)
  where
    lastRow = length rawGrid - 1
    lastCol = length (head rawGrid) - 1

allPaths :: Grid -> [[Coord]]
allPaths grid = go (lastRow, lastCol)
  where
    (_, (lastRow, lastCol)) = bounds grid
    go coord
      | coord == (0, 0) = [[coord]]
      | otherwise = do
          next <- nextPoints grid coord
          path <- go next
          return (coord : path)

nextPoints :: Grid -> Coord -> [Coord]
nextPoints grid (px, py) =
  filter (inRange (bounds grid)) [(px - 1, py), (px, py - 1)]
