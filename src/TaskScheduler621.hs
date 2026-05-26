module TaskScheduler621
  (time) where

import Data.List (group, sort)
import Data.Function

data Element =  Empty | Full deriving (Ord, Show, Eq)

merge :: Element -> Element -> Element
merge Empty e = e
merge e Empty = e
merge Full Full = Full

frequenciesDescending :: Ord a => [a] -> [Int]
frequenciesDescending = reverse . sort . map length . group . sort

time :: [String] -> Int -> Int
time tasks jump = frequenciesDescending tasks
    & map (createList jump)
    & foldl1 mergeLists
    & length

createList :: Int -> Int -> [Element]
createList _ 1 = [Full]
createList jump freq = Full : (take jump (repeat Empty)) ++ createList jump (pred freq)

mergeLists :: [Element] -> [Element] -> [Element]
mergeLists [] l2 = l2
mergeLists l1 [] = l1
mergeLists l1@(e1:rest1) l2@(e2:rest2) = 
    if e1 == Full && e2 == Full
        then mergeLists l1 $ Empty:l2
        else merge e1 e2 : mergeLists rest1 rest2
