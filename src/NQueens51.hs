module NQueens51 (calculateNQueens) where

import Data.Array
import Data.Function
import Data.List
import Lib

type Row = Int
type Column = Int
data Cell = Queen | Open | Closed
type Board =  [[(Row, Column, Cell)]]

-- calculateNQueens :: Int -> Int
calculateNQueens n = calculateNQueens' (createGrid n)

calculateNQueens' board = do
    row <- board
    (x, y, cell) <- row
    if cell == Open
        then calculateNQueens (addQueen x y board)
        else board

addQueen :: Int -> Int -> Board -> Board
addQueen x y board = board --TODO

createGrid :: Int -> Board
createGrid n = [[(x,y,Open) | x <- [0..n]] | y <- [0..n]]


