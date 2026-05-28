module AoC2025D10P2
  (
    Status(..)
  , main
  , parseJoltage
  , parseButtons
  , smashButtons
  , machine2
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
    contents <- readFile "src/AoC2025D10.input.mini.txt"
    print $ part2 contents
    
part2 input = 
    input
    & lines
    & map machine2

machine2 line = smashButtons sortedButtons Set.empty joltages
    & map Set.toList
    & map (map snd)
    & map length
    & minimum

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
    
parseJoltage :: [Char] -> [Light]
parseJoltage line = 
    splitOn " " line
    & last
    & init . tail
    & splitOn ","
    & map read

smashButtons :: [Button] -> Set.Set Button -> [Light] -> [Set.Set (Button, Int)]
smashButtons sortedButtons _ target =
    smashButtonsMemo (sortedButtons, ([], target))
    & map Set.fromList

smashButtonsMemo :: ([Button], ([(Button, Int)], [Light])) -> [[(Button, Int)]]
smashButtonsMemo = memo smashButtonsImpl

smashButtonsImpl :: ([Button], ([(Button, Int)], [Light])) -> [[(Button, Int)]]
smashButtonsImpl (sortedButtons, (pressedList, target)) =
    if invalid current target
        then []
        else if current == target
        then [pressedList]
        else
            concatMap
            (\button -> smashButtonsMemo (sortedButtons, (insertCount button pressedList, target)))
            sortedButtons
    where
        current = sumPresses pressedList (length target)

insertCount :: Button -> [(Button, Int)] -> [(Button, Int)]
insertCount button [] = [(button, 1)]
insertCount button ((b,c):rest)
    | button == b = (b, c+1) : rest
    | button < b  = (button, 1) : (b,c) : rest
    | otherwise   = (b,c) : insertCount button rest

invalid current target = or $
    zip current target
    & map (\(c,t) -> c > t)

sumPresses :: [(Button, Int)] -> Int -> [Light]
sumPresses buttonCounts len =
    foldl
        (\arr (button, count) ->
            foldl (\a i -> take i a ++ [a !! i + count] ++ drop (succ i) a) arr button)
        (replicate len 0)
        buttonCounts
