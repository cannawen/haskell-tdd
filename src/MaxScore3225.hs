module MaxScore3225 
    ( maxScore
    , getScoreAt
    , getColScore
    , scoreGrid) where

import Lib
import Data.Array
import Control.Monad
import Data.Function
import Data.List

type Row = Int
type Column = Int
type Score = Int
type Coord = (Row, Column)
type Grid = Array Coord Score

-- maxScore :: [[Int]] -> Int
maxScore rawGrid = grid
    & constructAllPossibleFills
    & map (scoreGrid grid)
    & maximum
    where grid = listToArray rawGrid

constructAllPossibleFills :: Grid -> [[Coord]]
constructAllPossibleFills grid =
    map concat (sequence allColFills)
    where
        (_, (xMax, yMax)) = bounds grid
        allColFills = [colFills col | col <- [0..yMax]]
        colFills col = [[(x, col) | x <- [0..height]] | height <- [-1..xMax]]

scoreGrid :: Grid -> [Coord] -> Int
scoreGrid grid blackSpaces = 
    [scoreIndex grid blackSpaces x y | x <- [0..xMax], y <- [0..yMax]]
    & foldl1' (+)
    where 
        (_, (xMax, yMax)) = bounds grid
        scoreIndex :: Grid -> [Coord] -> Row -> Column -> Score
        scoreIndex grid blackSpaces x y = 
            if not (elem (x, y) blackSpaces) && ( elem (x, y-1) blackSpaces || elem (x, y+1) blackSpaces)
                then grid ! (x , y)
                else 0

getColScore :: Grid -> Row -> Column -> Score
getColScore grid row col = 
    [getScoreAt grid x col | x <- [0..row]]
    & foldl1' (+)

getScoreAt :: Grid -> Row -> Column -> Score
getScoreAt grid row col = 
    if row >= 0 && col >= 0 && row <= xMax && col <= yMax
        then grid ! (row, col)
        else 0
    where (_, (xMax, yMax)) = bounds grid
