module NQueens51 
    (calculateNQueens,
    isValidInDir) where

import Data.Array
import Data.Function
import Data.List
import Lib
import Control.Applicative (Alternative(empty))

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
deltas = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

isValidInDir :: Int -> [(Row, Column)] -> (Row, Column) -> (Row, Column) -> Bool
isValidInDir n board start dir = 
    (r >= n || c >= n || r < 0 || c < 0 ) ||
    not (index `elem` board)  && isValidInDir n board index dir
    where index@(r, c) = (fst start + fst dir, snd start + snd dir)

-------------------------------------------------------------------------------

-- nQueens :: BoardSize -> Int
nQueens n = board3
    where
        queenPosRow0 = [(0,0),(0,1),(0,2),(0,3)]
        board0 = [[(0,0)],[(0,1)],[(0,2)],[(0,3)]]

        queenPosRow1 = [(1,0),(1,1),(1,2),(1,3)]
        board1 = concatMap (\board -> map (\q -> if isValid 4 (q:board) then q:board else []) queenPosRow1) board0 & filter (not . null)

        queenPosRow2 = [(2,0),(2,1),(2,2),(2,3)]
        board2 = concatMap (\board -> map (\q -> if isValid 4 (q:board) then q:board else []) queenPosRow2) board1 & filter (not . null)

        queenPosRow3 = [(3,0),(3,1),(3,2),(3,3)]
        board3 = concatMap (\board -> map (\q -> if isValid 4 (q:board) then q:board else []) queenPosRow3) board2 & filter (not . null)
