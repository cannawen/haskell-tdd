module NQueens51 (calculateNQueens) where

import Data.Array
import Data.Function
import Data.List
import Lib

type Row = Int
type Column = Int
type QueensRemaining = Int
data Cell = Queen | Valid | Invalid deriving (Eq)

instance Show Cell where
  show Queen   = "Q"
  show Valid   = " "
  show Invalid = "."

type Board =  [[Cell]]

-- calculateNQueens :: Int -> Int
calculateNQueens n = calculateNQueens' (n-1) (createGrid (n-1)) & length

calculateNQueens' :: Int -> Board -> [Board]
calculateNQueens' n board =
    if countQueens board == n + 1
        then [board]
        else concatMap (\board -> calculateNQueens' n board) possibleFutures
    where
        nextRow = countQueens board
        openCells = [(nextRow, y) | y <- [0..n], board !! nextRow !! y == Valid]
        possibleFutures = map (\(x,y) -> addQueen n x y board) openCells

countQueens board = filter (\c -> c == Queen) (concat board) & length

addQueen :: Int -> Row -> Column -> Board -> Board
addQueen n x y board = [[newCell r c | c <- [0..n]] | r <- [0..n]]
  where
    newCell r c
      | r == x && c == y = Queen
      | r == x || c == y = Invalid
      | abs (r - x) == abs (c - y) = Invalid
      | otherwise = board !! r !! c

createGrid :: Int -> Board
createGrid n = [[Valid | x <- [0..n]] | y <- [0..n]]

-------------------------------------------------------------------------------

calc :: Int -> Int
calc n = calc' (permutations [0..n-1])

calc' :: [[Int]] -> Int
calc' configurations = filter isValid configurations & length

isValid :: [Int] -> Bool
isValid configuration = False -- TODO
  where qCoords = zip [0..] configuration
