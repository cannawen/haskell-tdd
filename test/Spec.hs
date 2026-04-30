module Main (main) where

import Test.Hspec
import Lib (minOperations2033)

invalid :: Int
invalid = -1

main :: IO ()
main = hspec $ do
  describe "minOperations2033" $ do
    describe "invalid input" $ do
      describe "empty input" $ do
        it "returns invalid" $ do
          minOperations2033 [[]] 1 `shouldBe` invalid
      describe "unreachable input" $ do
        it "returns invalid" $ do
          minOperations2033 [[5,3]] 3 `shouldBe` invalid
      describe "no steps" $ do
        it "returns invalid" $ do
          minOperations2033 [[5,3]] 0 `shouldBe` invalid

    describe "valid input" $ do
      describe "single input" $ do
        it "returns 0" $ do
          minOperations2033 [[0]] 1 `shouldBe` 0
          minOperations2033 [[1]] 1 `shouldBe` 0
      describe "one step away" $ do
        it "returns 1" $ do
          minOperations2033 [[4,2]] 2 `shouldBe` 1
          minOperations2033 [[5,3]] 2 `shouldBe` 1