module GenerateParentheses22 where

import Control.Applicative
import Control.Monad
import Control.Monad.State

type ParensCount = Int
type OpenCount = Int
type ClosedCount = Int

generateT :: ParensCount -> [String]
generateT n = evalStateT (generateT' n) (0, 0)

generateT' :: ParensCount -> StateT (OpenCount, ClosedCount) [] String
generateT' n = do 
    (open, close) <- get
    if open == n && close == n 
        then return ""
        else addOpen open close <|> addClose open close
    where
        addOpen open close = do
            guard (open < n)
            put (open + 1, close)
            rest <- generateT' n
            return ('(':rest)
        addClose open close = do 
            guard (close < open)
            put (open, close + 1)
            rest <- generateT' n
            return (')':rest)


generateL :: ParensCount -> [String]
generateL n = generateL' n 0 0

generateL' :: ParensCount -> OpenCount -> ClosedCount -> [String]
generateL' n open close
    | open == n && close == n = return ""
    | otherwise = addOpen <|> addClose'
    where
        addOpen = do
            guard (open < n)
            rest <- generateL' n (open + 1) close
            return ('(':rest)
        addClose = do
            guard (close < open)
            rest <- generateL' n open (close + 1)
            return (')':rest)
        addClose' = 
            if (close < open)
                then map (')':) (generateL' n open (close + 1))
                else []

generateLC :: ParensCount -> [String]
generateLC n = generateLC' n 0 0

generateLC' :: ParensCount -> OpenCount -> ClosedCount -> [String]
generateLC' n open close
    | open == n && close == n = [""]
    | otherwise = 
        ['(' : rest | open < n, rest <- generateLC' n (open+1) close]
        ++ 
        [')' : rest | close < open, rest <- generateLC' n open (close+1)]
