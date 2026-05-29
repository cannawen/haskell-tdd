module AoC2025D10P22
  (
    Status(..)
  , main
  , parseJoltage
  , parseButtons
  , buttonsToJoltage
  , possibleButtonCombos
  , maxButtonPresses
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
type Joltage = Int
type Button = [Joltage]

main = do
    contents <- readFile "src/AoC2025D10.input.mini.txt"
    print $ part2 contents
    
part2 input = 
    input
    & lines
    & map machine2

-- machine2 :: [Char] -> Int
machine2 line =  possibleButtonCombos1
    & filter (doButtonsMatchJoltage joltages)
    & map (length . concat)
    & minimum

    where
        joltages = parseJoltage line
        sortedButtons = parseButtons line
            & sortOn length
            & reverse
        maxPressCount = map (\b -> maxButtonPresses b joltages) sortedButtons
        possibleButtonCombos1 = 
            map (\(button, maxCount) -> do
                count <- [0..maxCount]
                return (take count (repeat button))
            ) (zip sortedButtons maxPressCount)
            & sequence


possibleButtonCombos :: [Joltage] -> [Button] -> [[Button]]
possibleButtonCombos joltage availableButtons
    | joltage == replicate (length joltage) 0 = [[]]
    | any (< 0) joltage = []
    | null availableButtons = []
    | otherwise = do
        let b = head availableButtons
        buttonPresses <- [0.. maxButtonPresses b joltage]
        let pressedButtons = replicate buttonPresses b
        let newJoltage = calcNewJoltage pressedButtons joltage
        restCombos <- possibleButtonCombos newJoltage (tail availableButtons)
        return (pressedButtons ++ restCombos)

doButtonsMatchJoltage :: [Joltage] -> [[Button]] -> Bool
doButtonsMatchJoltage joltage buttons = 
    joltage == buttonsToJoltage lights size
    where
        size = length joltage
        lights = concat buttons

calcNewJoltage :: [Button] -> [Joltage] -> [Joltage]
calcNewJoltage buttonsPressed oldJoltage = 
    map (\(old, pressed) -> old - pressed) (zip oldJoltage buttonJoltage)
    where buttonJoltage = buttonsToJoltage buttonsPressed (length oldJoltage)

buttonsToJoltage :: [Button] -> Int -> [Joltage]
buttonsToJoltage buttons size = 
    map (\i -> length (filter (== i) flatButtons)) [0..pred size]
    where
        flatButtons = concat buttons

maxButtonPresses :: Button -> [Joltage] -> Int
maxButtonPresses button joltage = minimum $ map (\i -> joltage !! i) button

parseButtons :: [Char] -> [Button]
parseButtons line = 
    line
    & splitOn " "
    & tail . init
    & map (map read . splitOn "," . tail . init)
    
parseJoltage :: [Char] -> [Joltage]
parseJoltage line = 
    splitOn " " line
    & last
    & init . tail
    & splitOn ","
    & map read

