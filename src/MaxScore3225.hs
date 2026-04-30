module MaxScore3225 (maxScore) where

import Data.Array

maxScore :: [[Int]] -> Int
maxScore rawGrid = maximum [dp ! (n-1, d) | d <- [0..n]]
  where
    n = length rawGrid
    grid = listArray ((0,0), (n-1,n-1)) (concat rawGrid) :: Array (Int,Int) Int

    -- pre[j][k] = sum of col j rows 0..k-1
    pre :: Array (Int,Int) Int
    pre = listArray ((0,0), (n-1,n))
      [ if k == 0 then 0 else pre ! (j, k-1) + grid ! (k-1, j)
      | j <- [0..n-1], k <- [0..n] ]

    colSum j lo hi = pre ! (j, hi) - pre ! (j, lo)

    -- dp[j][d] = max score for cols 0..j with col j at depth d
    dp :: Array (Int,Int) Int
    dp = listArray ((0,0), (n-1,n))
      [ if j == 0 then 0
        else maximum [ dp ! (j-1, p) + trans p d j | p <- [0..n] ]
      | j <- [0..n-1], d <- [0..n] ]

    -- score gained when col j-1 has depth p and col j has depth d
    trans p c j
      | p > c    = colSum j c p        -- white in col j [c..p-1] with black left
      | c > p    = colSum (j-1) p c    -- white in col j-1 [p..c-1] with black right
      | otherwise = 0
