module MaximumTotalSubarrayValueI3689 where

import Data.Function
import Data.List (sort)

maxValues :: [Int] -> Int -> Int
maxValues nums k = 
    allSubarr nums
    & map value
    & sort
    & reverse
    & take k
    & sum

allSubarr :: [Int] -> [[Int]]
allSubarr nums = do
    l <- [0..pred len]
    r <- [succ l..len]
    return (drop l nums & take r)
    where len = length nums

value :: [Int] -> Int
value nums = (maximum nums) - (minimum nums)
