module Lib
    ( listToArray
    , rotateBy) where

import Data.Array

listToArray :: [[e]] -> Array (Int, Int) e
listToArray list = array ((0,0), (rows - 1, cols - 1)) associationList
    where 
        rows = length list
        cols = length (head list)
        associationList =
            [ ((r, c), val)
            | (r, row) <- zip [0..] list
            , (c, val) <- zip [0..] row]


rotateBy :: Int -> [a] -> [a]
rotateBy n arr = drop n arr ++ take n arr