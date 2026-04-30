module Lib
  (minOperations2033
  ) where
import Data.Function
import Data.List

minOperations2033 :: [[Int]] -> Int -> Int
minOperations2033 grid step = minOperations array step
  where
    array = grid & concat & sort

minOperations :: [Int] -> Int -> Int
minOperations [_] _ = 0
minOperations [] _ = -1
minOperations _ 0 = -1
minOperations xs step =
  if allSame (map snd dm)
    then foldl1' (+) (map fst dm)
    else (-1)
  where
    med = median xs
    dm = map (\x -> abs (x - med) `divMod` step ) xs

median :: [Int] -> Int
median array = array !! ((length array) `div` 2)

allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs
