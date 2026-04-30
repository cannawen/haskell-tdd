module Main (main) where

import Lib (add, multiply)

main :: IO ()
main = do
  -- add tests
  assert "1 + 1 = 2"   (add 1 1 == 2)
  assert "0 + 5 = 5"   (add 0 5 == 5)
  assert "(-3) + 3 = 0" (add (-3) 3 == 0)

  -- multiply tests
  assert "2 * 3 = 6"   (multiply 2 3 == 6)
  assert "0 * 9 = 0"   (multiply 0 9 == 0)
  assert "(-2) * 4 = -8" (multiply (-2) 4 == -8)

  putStrLn "All tests passed!"

assert :: String -> Bool -> IO ()
assert name True  = putStrLn $ "PASS: " ++ name
assert name False = error  $ "FAIL: " ++ name
