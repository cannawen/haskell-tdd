module GenerateParentheses22 where

import Control.Applicative
import Control.Monad
import Control.Monad.State

type OpenCount = Int
type ClosedCount = Int

generate :: Int -> [String]
generate n = evalStateT (generateT n) (0, 0)

generateT :: Int -> StateT (OpenCount, ClosedCount) [] String
generateT n = do 
    (open, close) <- get
    if open == n && close == n 
        then return ""
        else addOpen open close <|> addClose open close
    where
        addOpen open close = do
            guard (open < n)
            put (open + 1, close)
            rest <- generateT n
            return ('(':rest)
        addClose open close = do 
            guard (close < open)
            put (open, close + 1)
            rest <- generateT n
            return (')':rest)
