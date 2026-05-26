module AoC2025D9
  (main, part1) where

import Data.Array
import Data.Function
import Data.List
import Data.List.Split
import Lib

data Status = Off | On deriving (Eq, Ord, Show)

main = do
    contents <- readFile "src/AoC2025D10.input.txt"
    print $ part1 contents
    
part1 input = 
    input
    & lines
    & map machine

machine line = targetIndicator line

targetIndicator :: [Char] -> [Status]
targetIndicator line =
    line 
    & splitOn " "
    & head
    & foldr (\c memo -> if c == '.' then Off:memo else if c == '#' then On:memo else memo) []