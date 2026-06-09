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
    return (drop l nums & take (r-l))
    where len = length nums

allSubarr'' :: [Int] -> [[Int]]
allSubarr'' nums = [drop l nums & take (r-l) | l <- [0..pred len], r <- [succ l..len]]
    where len = length nums

allSubarr' :: [Int] -> [[Int]]
allSubarr' nums = 
    map 
    (\l -> 
        map 
        (\r -> 
            drop l nums & take (r-l)) 
        [succ l..len]
    ) 
    [0..pred len]
    & concat

    where len = length nums

value :: [Int] -> Int
value nums = (maximum nums) - (minimum nums)
