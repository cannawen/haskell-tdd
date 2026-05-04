module NQueens51 
    (nQueens,
    isValidInDir) where

import Data.Array
import Data.Function
import Data.List
import Lib
import Control.Applicative (Alternative(empty))
import Control.Monad

-- Very not functional; represent each cell in board -----------------------------------------------------------------------------

type Row = Int
type Column = Int
type QueensRemaining = Int
data Cell = Queen | Valid | Invalid deriving (Eq)

instance Show Cell where
  show Queen   = "Q"
  show Valid   = " "
  show Invalid = "."

type Board =  [[Cell]]

nQueens n = nQueens' (n-1) (createGrid (n-1)) & length

nQueens' :: Int -> Board -> [Board]
nQueens' n board =
    if countQueens board == n + 1
        then [board]
        else concatMap (\board -> nQueens' n board) possibleFutures
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

-- Brute force check all solutions with one Q per row/column -----------------------------------------------------------------------------

type BoardSize = Int
type QueenCoordinate = (Row, Column)

nQueens'' :: BoardSize -> Int
nQueens'' n = nQueens''' n $ map (\p -> zip [0..] p) (permutations [0..n-1])

nQueens''' :: BoardSize -> [[QueenCoordinate]] -> Int
nQueens''' n possibleBoards = filter (isValid n) possibleBoards & length

isValid :: BoardSize -> [QueenCoordinate] -> Bool
isValid n board = and $ map (\coord -> map (isValidInDir n board coord) deltas & and) board

isValid' :: BoardSize -> QueenCoordinate -> [QueenCoordinate] -> Bool
isValid' n potentialQueen board = map (isValidInDir n board potentialQueen) deltas & and

deltas :: [(Row, Column)]
deltas = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

isValidInDir :: Int -> [(Row, Column)] -> (Row, Column) -> (Row, Column) -> Bool
isValidInDir n board start dir = 
    (r >= n || c >= n || r < 0 || c < 0 ) ||
    not (index `elem` board)  && isValidInDir n board index dir
    where index@(r, c) = (fst start + fst dir, snd start + snd dir)

-- Wasn't sure how to to a map over the solution; hard-coded for 4 -----------------------------------------------------------------------------

nQueens'''' n = board3
    where
        queenPosRow0 = [(0,0),(0,1),(0,2),(0,3)]
        board0 = [[(0,0)],[(0,1)],[(0,2)],[(0,3)]]

        queenPosRow1 = [(1,0),(1,1),(1,2),(1,3)]
        board1 = concatMap (\board -> map (\q -> if isValid' 4 q board then q:board else []) queenPosRow1) board0 & filter (not . null)

        queenPosRow2 = [(2,0),(2,1),(2,2),(2,3)]
        board2 = concatMap (\board -> map (\q -> if isValid' 4 q board then q:board else []) queenPosRow2) board1 & filter (not . null)

        queenPosRow3 = [(3,0),(3,1),(3,2),(3,3)]
        board3 = concatMap (\board -> map (\q -> if isValid' 4 q board then q:board else []) queenPosRow3) board2 & filter (not . null)

-- Transformed the hard coded stuff into a loop -----------------------------------------------------------------------------

nQueens''''' n = 
    foldl' 
    (\boards row ->
        concatMap 
        (\board -> 
            map 
            (\q -> 
                if isValid' n q board then q:board else []) 
            (map (\col -> (row, col)) [0..n-1]))
        boards
        & filter (not . null)
    ) [[]] [0..n-1]

-- Turn the loop into using monads -----------------------------------------------------------------------------

nQueens'''''' n = foldl' expand [[]] [0..n-1] & length
    where
        expand boards row = do
            board <- boards
            col <- [0..n-1]
            if isValid' n (row,col) board
                then return ((row,col) : board)
                else []

-- Monadic reduction & guard -----------------------------------------------------------------------------

nQueens''''''' n = foldM expand [] [0..n-1] & length
    where
        expand board row = do
            col <- [0..n-1]
            guard $ isValid' n (row,col) board
            return ((row,col) : board)

-- List Comprehension -----------------------------------------------------------------------------

nQueens'''''''' n = foldl' expand [[]] [0..n-1] & length
    where
        expand boards row = 
            [(row, col) : board
            | board <- boards
            , col <- [0..n-1]
            , isValid' n (row,col) board]

-- TODO: the `isValid'` fn can be simplified since we are only placing one Q per row -----------------------------------------------------------------------------
