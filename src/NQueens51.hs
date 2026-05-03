module NQueens51 (calculateNQueens) where

import Data.Array
import Data.Function
import Data.List
import Lib

type Row = Int
type Column = Int
data Cell = Queen | Open | Closed
type Board =  Array (Row, Column) Cell

calculateNQueens :: Int -> Int
calculateNQueens n = 0

createGrid :: Int -> Board
createGrid n = listArray ((1,1), (n,n)) (repeat Open)


