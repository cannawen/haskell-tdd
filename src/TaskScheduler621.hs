module TaskScheduler621
  (time) where

import Data.List (group, sort)
import Data.Function

data Element =  Empty | Full deriving (Show, Eq)

frequenciesDescending :: Ord a => [a] -> [Int]
frequenciesDescending = reverse . sort . map length . group . sort

createList :: Int -> Int -> [Element]
createList _ 1 = [Full]
createList jump freq = Full : (take jump (repeat Empty)) ++ createList jump (pred freq)

merge :: Element -> Element -> Element
merge Empty e = e
merge e Empty = e
merge Full Full = Full

mergeLists :: [Element] -> [Element] -> [Element]
mergeLists [] list2 = list2
mergeLists list1 [] = list1
mergeLists list1@(head1:tail1) list2@(head2:tail2) =
    if head1 == Full && head2 == Full
        then mergeLists list1 (Empty:list2)
        else merge head1 head2 : mergeLists tail1 tail2

time :: [String] -> Int -> Int
time tasks jump = frequenciesDescending tasks
    & map (createList jump)
    & foldl1 mergeLists
    & length
