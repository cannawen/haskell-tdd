module Main (main) where

import Test.Hspec
import Lib (add, multiply)

main :: IO ()
main = hspec $ do
  describe "add" $ do
    it "adds two positive numbers" $
      add 1 1 `shouldBe` 2
    it "handles zero" $
      add 0 5 `shouldBe` 5
    it "handles negative numbers" $
      add (-3) 3 `shouldBe` 0

  describe "multiply" $ do
    it "multiplies two numbers" $
      multiply 2 3 `shouldBe` 6
    it "multiplies by zero" $
      multiply 0 9 `shouldBe` 0
    it "handles negative numbers" $
      multiply (-2) 4 `shouldBe` (-8)
