module AoC2025D9
  (main, part1) where

import Data.Array
import Data.Function
import Data.List
import Data.List.Split
import Lib

data Status = Off | Toggle | On deriving (Eq, Show)

main = do
    contents <- readFile "src/AoC2025D10.input.txt"
    print $ part1 contents
    
part1 input = 
    input
    & lines
    & map machine

machine line = 
    -- targetIndicator line
    buttons line

targetIndicator :: [Char] -> [Status]
targetIndicator line =
    line 
    & splitOn " "
    & head
    & foldr (\c memo -> if c == '.' then Off:memo else if c == '#' then On:memo else memo) []

buttons :: [Char] -> [[Status]]
-- buttons :: String -> [[Int]]
buttons line = 
    line
    & splitOn " "
    & tail . init
    & map (map read . splitOn "," . tail . init)
    & map createButton

createButton :: [Int] -> [Status]
createButton positions = 
    foldl (\memo i -> placeToggleAtIndexInArray i memo) [] positions

placeToggleAtIndexInArray :: Int -> [Status] -> [Status]
placeToggleAtIndexInArray i arr = 
    if length arr > i 
        then take i arr ++ [Toggle] ++ (drop i arr)
        else arr ++ (take (i - length arr) (repeat Off)) ++ [Toggle]
