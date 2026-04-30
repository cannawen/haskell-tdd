module MaxPath3742 
  (maxPath) where
import Data.Maybe (catMaybes, maybeToList)
import Data.Function
import Data.List
import Control.Monad

type Row = Int
type Column = Int
type Cost = Int
type Point = (Row, Column, Cost)

pathScores :: [Cost] -> (Int, Int)
pathScores path = foldl' updateFn (0,0) path
  where
    updateFn (score, cost) 2 = (score + 2, cost + 1)
    updateFn (score, cost) 1 = (score + 1, cost + 1)
    updateFn (score, cost) _ = (score, cost)

maxPath :: [[Int]] -> Int -> Int
maxPath grid maxCost = if not (null p) then maximum p else -1
  where 
    rawPathCosts = map (\path -> (map (\(_,_,cost) -> cost) path)) (allPaths (parse grid)) 
    p = 
      map pathScores rawPathCosts 
      & filter (\(_, cost) -> cost <= maxCost) 
      & map fst

parse :: [[Int]] -> [[Point]]
parse grid = [[(x, y, cost) | (y, cost) <- zip [0..] row] | (x, row) <- zip [0..] grid]

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
nextPoints grid (px, py, pcost) = do
  (dx, dy) <- [(-1, 0), (0, 1)]
  maybeToList (getPointInGrid grid (px + dx) (py + dy))
  
getPointInGrid :: [[Point]] -> Row -> Column -> Maybe Point
getPointInGrid grid x y = do
  guard (x >= 0 && y >= 0)
  return $ grid !! x !! y
  