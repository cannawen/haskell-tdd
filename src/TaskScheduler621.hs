module TaskScheduler621
  (time) where

time :: [String] -> Int -> Int
time tasks n = 
    if length tasks == 1
        then 1
        else length tasks * succ n
