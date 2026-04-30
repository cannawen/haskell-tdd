module Main (main) where

import Lib (add, multiply)

main :: IO ()
main = do
  putStrLn $ "1 + 1 = " ++ show (add 1 1)
  putStrLn $ "2 * 3 = " ++ show (multiply 2 3)
