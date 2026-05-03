module RotateFunction396 () where
import Data.Function
import Data.List

rotate :: [Int] -> Int
rotate arr = 
    [zip [0..] (rotateBy n arr) | n <- [0..length arr]]
    & map calculateProductSum 
    & maximum
    
    where
        rotateBy :: Int -> [a] -> [a]
        rotateBy n arr = drop n arr ++ take n arr
        
        calculateProductSum :: [(Int,Int)] -> Int
        calculateProductSum arr = map (\(i,n) -> i * n ) arr & foldl1' (+)
