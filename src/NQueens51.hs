module NQueens51 
    (calculateNQueens,
    isValidInDir) where

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

type BoardSize = Int
type QueenCoordinate = (Row, Column)

calc :: BoardSize -> Int
calc n = calc' n $ map (\p -> zip [0..] p) (permutations [0..n-1])

calc' :: BoardSize -> [[QueenCoordinate]] -> Int
calc' n possibleBoards = filter (isValid n) possibleBoards & length

isValid :: BoardSize -> [QueenCoordinate] -> Bool
isValid n board = and $ map (\coord -> map (isValidInDir n board coord) deltas & and) board

deltas :: [(Row, Column)]
deltas = [(dx, dy) | dx <- [-1, 1], dy <- [-1, 1]]

isValidInDir :: Int -> [(Row, Column)] -> (Row, Column) -> (Row, Column) -> Bool
isValidInDir n board start dir = 
    (r >= n || c >= n || r < 0 || c < 0 ) ||
    not (index `elem` board)  && isValidInDir n board index dir
    where index@(r, c) = (fst start + fst dir, snd start + snd dir)