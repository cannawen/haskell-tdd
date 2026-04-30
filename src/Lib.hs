module Lib
  (minOperations2033
  ) where
import Data.Function

minOperations2033 :: [[Int]] -> Int -> Int
minOperations2033 grid step = minOperations array step
  where 
    array = grid & concat

minOperations :: [Int] -> Int -> Int
minOperations [x] _ = 0
minOperations _ _ = -1
