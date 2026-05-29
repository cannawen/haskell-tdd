module AoC2025D10P22
  (
    Status(..)
  , main
  , parseJoltage
  , parseButtons
  , buttonsToJoltage
  , buttonDelta
  , findComboCount
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
    contents <- readFile "src/AoC2025D10.input.txt"
    print $ part2 contents
    
part2 input = 
    input
    & lines
    & map machine2
    & sum

-- machine2 :: [Char] -> Int
machine2 line = case findComboCount joltageArr buttonsWithDeltas of
    Just n  -> n
    Nothing -> 0
    where
        joltages = parseJoltage line
        size = length joltages
        joltageArr = listArray (0, size - 1) joltages
        buttons = parseButtons line & sortOn length & reverse
        buttonsWithDeltas = map (\b -> (b, buttonDelta size b)) buttons

buttonDelta :: Int -> Button -> Array Int Int
buttonDelta size button = accumArray (+) 0 (0, size - 1) [(i, 1) | i <- button]

findComboCount :: Array Int Int -> [(Button, Array Int Int)] -> Maybe Int
findComboCount joltage buttonsWithDeltas
    | all (== 0) (elems joltage) = Just 0
    | any (< 0) (elems joltage) = Nothing
    | not (coverable joltage (map snd buttonsWithDeltas)) = Nothing
    | otherwise =
        let (b, delta, rest) = pickMostConstrained joltage buttonsWithDeltas
            maxPresses = minimum (map (joltage !) b)
            applyK k   = accum (\old d -> old - k * d) joltage
                             [(i, d) | (i, d) <- assocs delta, d > 0]
            go k
                | k < 0     = Nothing
                | otherwise = case findComboCount (applyK k) rest of
                    Just count -> Just (count + k)
                    Nothing    -> go (k - 1)
        in go maxPresses

pickMostConstrained :: Array Int Int -> [(Button, Array Int Int)] -> (Button, Array Int Int, [(Button, Array Int Int)])
pickMostConstrained joltage buttons =
    let pressesFor (b, _) = minimum (map (joltage !) b)
        (_, idx) = minimum $ zip (map pressesFor buttons) [0..]
        (before, chosen:after) = splitAt idx buttons
    in (fst chosen, snd chosen, before ++ after)

coverable :: Array Int Int -> [Array Int Int] -> Bool
coverable joltage deltas = all check (indices joltage)
  where check i = joltage ! i == 0 || any (\d -> d ! i > 0) deltas


possibleButtonCombos :: [Joltage] -> [Button] -> [[Button]]
possibleButtonCombos joltage availableButtons
    | joltage == replicate (length joltage) 0 = [[]]
    | any (< 0) joltage = []
    | null availableButtons = []
    | otherwise = do
        let b = head availableButtons
        buttonPresses <- reverse [0.. maxButtonPresses b joltage]
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

