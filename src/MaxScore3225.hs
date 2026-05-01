module MaxScore3225 
    ( maxScore
    , getScoreAt
    , getColScore) where

import Lib
import Data.Array
import Control.Monad
import Data.Function
import Data.List

type Row = Int
type Column = Int
type Cost = Int
type Coord = (Row, Column)
type Grid = Array Coord Cost

-- maxScore :: [[Int]] -> Int
maxScore grid = listToArray grid

getColScore grid row col = 
    [getScoreAt grid x y | x <- [0..row], y <- [col+1, col-1]]
    & foldl1' (+)

getScoreAt :: Grid -> Row -> Column -> Cost
getScoreAt grid row col = 
    if row >= 0 && col >= 0 && row <= xMax && col <= yMax
        then grid ! (row, col)
        else 0
    where (_, (xMax, yMax)) = bounds grid