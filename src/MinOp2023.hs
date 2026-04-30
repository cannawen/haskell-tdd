module MinOp2023
  (minOp
  ) where
import Data.Function
import Data.List

minOp :: [[Int]] -> Int -> Int
minOp grid step = minOperations array step
  where
    array = grid & concat & sort

minOperations :: [Int] -> Int -> Int
minOperations [_] _ = 0
minOperations [] _ = -1
minOperations _ 0 = -1
minOperations xs step =
  if allSame (map snd divModResult)
    then foldl1' (+) (map fst divModResult)
    else (-1)
  where
    med = xs !! ((length xs) `div` 2)
    divModResult = map (\x -> abs (x - med) `divMod` step ) xs

allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs
