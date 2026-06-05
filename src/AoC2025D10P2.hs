module AoC2025D10P2

  ( main
  , machine
  , parseButtons
  ) where

import Data.Array
import Data.Function
import Data.List
import Data.List.Split
import Lib
import Control.Applicative
import qualified Data.Set as Set

type Light = Int
type Button = [Light]

main = do
    contents <- readFile "src/AoC2025D10.input.txt"
    print $ part1 contents
    
part1 input = 
    input
    & lines
    & map machine

machine line = sumRows pressedButtons

    where
        target = parseTarget line
        buttons = parseButtons line
        pressedButtons = mapM (\b -> [b] <|> [[0,0,0,0]]) buttons

sumRows :: [[[Int]]] -> [[Int]]
sumRows = map sumRow
  where
    sumRow :: [[Int]] -> [Int]
    sumRow = foldl (\acc x -> zipWith (+) acc x) [0, 0, 0, 0] -- Need to tack on info here for how many buttons were pressed

parseTarget :: [Char] -> [Light]
parseTarget line =
    line 
    & splitOn " "
    & head
    & foldr (\c memo -> if c == '.' then 0:memo else if c == '#' then 1:memo else memo) []

parseButtons :: [Char] -> [Button]
parseButtons line = expandedButtons
    where 
        indices =
            line
            & splitOn " "
            & tail . init
            & map (map read . splitOn "," . tail . init)
            
        expandedButtons = indices 
            & map Set.fromList
            & map toLights

        max = concat indices & maximum

        toLights :: Set.Set Light -> [Light]
        toLights buttonIndex = map (\i -> if Set.member i buttonIndex then 1 else 0) [0..max]
