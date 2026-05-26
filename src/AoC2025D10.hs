module AoC2025D9
  (main, machine) where

import Data.Array
import Data.Function
import Data.List
import Data.List.Split
import Lib
import Control.Applicative

data Status = Off | Toggle | On deriving (Eq, Ord, Show)

main = do
    contents <- readFile "src/AoC2025D10.input.txt"
    print $ part1 contents
    
part1 input = 
    input
    & lines
    & map machine

machine line = pressedButtons
    & map ( foldl (\(m,c) b -> (mergeList m b, length b)) (startingState, 0) )
    & filter (\(output, _) -> output == target)
    -- & map snd
    -- & sort
    -- & head

    where
        startingState = take (length target) (repeat Off)
        target = parseTarget line
        buttons = parseButtons line
        pressedButtons = mapM (\b -> [b] <|> [[]]) buttons

mergeStatus :: Status -> Status -> Status
mergeStatus s1 s2 = mergeStatusSorted (min s1 s2) (max s1 s2)

mergeStatusSorted :: Status -> Status -> Status
mergeStatusSorted Off Toggle = On
mergeStatusSorted Toggle On = Off
mergeStatusSorted Off On = On
mergeStatusSorted s _ = s

mergeList :: [Status] -> [Status] -> [Status]
mergeList s1 [] = s1
mergeList [] s2 = s2
mergeList s1@(head1:tail1) s2@(head2:tail2) = mergeStatus head1 head2 : mergeList tail1 tail2

parseTarget :: [Char] -> [Status]
parseTarget line =
    line 
    & splitOn " "
    & head
    & foldr (\c memo -> if c == '.' then Off:memo else if c == '#' then On:memo else memo) []

parseButtons :: [Char] -> [[Status]]
parseButtons line = 
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
