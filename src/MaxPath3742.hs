module MaxPath3742 
  (maxPath) where
import Data.Maybe (catMaybes)
import Data.Function

type Row = Int
type Column = Int
type Cost = Int
type Point = (Row, Column, Cost)

maxPath :: [[Int]] -> Int -> Int
maxPath grid maxCost = -1

parse :: [[Int]] -> [[Point]]
parse grid = 
  map 
  (\(x, row) -> 
    map 
    (\(y, cost) -> (x,y,cost)) 
    (zip [0..] row)) 
  (zip [0..] grid)

nextPoints :: [[Point]] -> Point -> [Point]
nextPoints grid (px, py, pcost) =
  catMaybes 
  [getPointInGrid grid (px+1) py, getPointInGrid grid px (py+1)]

getPointInGrid :: [[Point]] -> Row -> Column -> Maybe Point
getPointInGrid grid x y = 
  if x < rowCount && y < columnCount 
    then Just $ grid !! x !! y
    else Nothing
  where
    rowCount = length grid
    columnCount = length $ head grid