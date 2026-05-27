module AoC2025D10P1
  (Status(..)
  , main
  , machine
  , parseButtons
  , allCombinationOfButtons
  , applyButtons) where

import Data.Array
import Data.Function
import Data.List
import Data.List.Split
import Lib
import Control.Applicative
import qualified Data.Set  as Set

data Status = Off | Toggle | On deriving (Eq, Ord, Show)
type Light = Int
type Button = [Light]

main = do
    contents <- readFile "src/AoC2025D10.input.txt"
    print $ part1 contents
    
part1 input = 
    input
    & lines
    & map machine
    & sum

machine line = pressedButtons
    & map (\b -> (applyButtons startingState b, length (filter (not . null) b)))
    & filter (\(b, _) -> b == target)
    & map snd
    & minimum
    where
        startingState = take (length target) (repeat Off)
        target = parseTarget line
        buttons = parseButtons line
        pressedButtons = mapM (\b -> [b] <|> [[]]) buttons
        -- pressedButtons = allCombinationOfButtons buttons

allCombinationOfButtons :: [Button] -> [[Button]]
allCombinationOfButtons buttons = foldr (\button memo -> fmap (button :) memo ++ memo) [[]] buttons

applyButtons :: [Status] -> [Button] -> [Status]
applyButtons startingState buttons  = 
    buttons
    & map createButton
    & foldl (\m b -> mergeList m b) startingState

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

parseButtons :: [Char] -> [Button]
parseButtons line = 
    line
    & splitOn " "
    & tail . init
    & map (map read . splitOn "," . tail . init)

createButton :: Button -> [Status]
createButton positions = 
    foldl (\memo i -> placeToggleAtIndexInArray i memo) [] positions

placeToggleAtIndexInArray :: Int -> [Status] -> [Status]
placeToggleAtIndexInArray i arr = 
    if length arr > i 
        then take i arr ++ [Toggle] ++ (drop i arr)
        else arr ++ (take (i - length arr) (repeat Off)) ++ [Toggle]
