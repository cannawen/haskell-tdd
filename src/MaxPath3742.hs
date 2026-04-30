module MaxPath3742 
  (maxPath) where
import Data.Maybe (catMaybes)
import Data.Function
import Data.List

type Row = Int
type Column = Int
type Cost = Int
type Point = (Row, Column, Cost)

updateFn (score, cost) 2 = (score + 2, cost + 1)
updateFn (score, cost) 1 = (score + 1, cost + 1)
updateFn (score, cost) _ = (score, cost)

pathScores :: [Cost] -> (Int, Int)
pathScores path = foldl' updateFn (0,0) path

maxPath :: [[Int]] -> Int -> Int
maxPath grid maxCost = if length p > 0 then maximum p else -1
  where 
    rawPathCosts = map (\path -> (map (\(_,_,cost) -> cost) path)) (allPaths (parse grid)) 
    p = map pathScores rawPathCosts & filter (\(_, cost) -> cost <= maxCost) & map fst

parse :: [[Int]] -> [[Point]]
parse grid = 
  map 
  (\(x, row) -> 
    map 
    (\(y, cost) -> (x,y,cost)) 
    (zip [0..] row)) 
  (zip [0..] grid)

allPaths :: [[Point]] -> [[Point]]
allPaths grid = go (grid !! lastRow !! lastCol)
  where
    lastRow = length grid - 1
    lastCol = length (head grid) - 1
    go point@(x,y,_)
      | x == 0 && y == 0 = [[point]]
      | otherwise = do
        next <- nextPoints grid point
        path <- go next
        return (point : path)

nextPoints :: [[Point]] -> Point -> [Point]
nextPoints grid (px, py, pcost) =
  catMaybes 
  [getPointInGrid grid (px-1) py, getPointInGrid grid px (py-1)]

getPointInGrid :: [[Point]] -> Row -> Column -> Maybe Point
getPointInGrid grid x y = 
  if x >= 0 && y >= 0 
    then Just $ grid !! x !! y
    else Nothing