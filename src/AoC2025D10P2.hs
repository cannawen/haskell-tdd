module AoC2025D10P2
  (Status(..)
  , main
  , parseButtons
  ) where

import Data.Array
import Data.Function
import Data.List
import Data.List.Split
import Lib
import Control.Applicative
import qualified Data.Set  as Set
import Data.MemoTrie (memo)

data Status = Off | Toggle | On deriving (Eq, Ord, Show)
type Light = Int
type Button = [Light]

main = do
    contents <- readFile "src/AoC2025D10.input.txt"
    print $ part2 contents
    
part2 input = 
    input
    & lines
    & map machine2

machine2 line = smashButtons sortedButtons Set.empty joltages
    
    where
        joltages = parseJoltage line
        sortedButtons = parseButtons line
            & sortOn length
            & reverse

parseButtons :: [Char] -> [Button]
parseButtons line = 
    line
    & splitOn " "
    & tail . init
    & map (map read . splitOn "," . tail . init)
    
parseJoltage :: [Char] -> [Int]
parseJoltage line = 
    splitOn " " line
    & last
    & init . tail
    & splitOn ","
    & map read

smashButtons :: [Button] -> Set.Set Button -> [Int] -> [Set.Set Button]
smashButtons sortedButtons buttonsPressed target =
    smashButtonsMemo (sortedButtons, (Set.toAscList buttonsPressed, target))
    & map Set.fromList

smashButtonsMemo :: ([[Int]], ([[Int]], [Int])) -> [[[Int]]]
smashButtonsMemo = memo smashButtonsImpl

smashButtonsImpl :: ([[Int]], ([[Int]], [Int])) -> [[[Int]]]
smashButtonsImpl (sortedButtons, (pressedList, target)) =
    if invalid current target
        then []
        else if current == target
        then [pressedList]
        else
            concatMap
            (\button -> smashButtonsMemo (sortedButtons, (Set.toAscList (Set.insert button (Set.fromList pressedList)), target)))
            sortedButtons
    where current = sumPresses (Set.fromList pressedList) (length target)

invalid current target = or $
    zip current target
    & map (\(c,t) -> c > t)

sumPresses :: Set.Set Button -> Int -> [Int]
sumPresses buttons len =
    buttons
    & Set.toList
    & concat
    & sort
    & group
    & map (\g -> (head g, length g))
    & foldl 
        (\m (i, count) -> (take i m) ++ [count] ++ (drop (succ i) m))
        (take len (repeat 0))
