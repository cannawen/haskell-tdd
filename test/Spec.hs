module Main (main) where

import Test.Hspec
import Lib (minOperations2033)

main :: IO ()
main = hspec $ do
  describe "minOperations2033" $ do
    describe "invalid input" $ do
      describe "empty input" $ do
        it "returns -1" $ do
          minOperations2033 [[]] 1 `shouldBe` -1
      describe "unreachable input" $ do
        it "returns -1" $ do
          minOperations2033 [[5,3]] 3 `shouldBe` -1
          minOperations2033 [[1,2],[3,4]] 2 `shouldBe` -1
      describe "no steps" $ do
        it "returns -1" $ do
          minOperations2033 [[5,3]] 0 `shouldBe` -1

    describe "valid input" $ do
      describe "single input" $ do
        it "returns 0" $ do
          minOperations2033 [[0]] 1 `shouldBe` 0
          minOperations2033 [[1]] 1 `shouldBe` 0
          minOperations2033 [[1]] 0 `shouldBe` 0
      describe "one step away" $ do
        it "returns 1" $ do
          minOperations2033 [[4,2]] 2 `shouldBe` 1
          minOperations2033 [[3,5]] 2 `shouldBe` 1
      describe "2D array should be flattened" $ do
        it "returns accurately" $ do
          minOperations2033 [[2,4],[6,8]] 2 `shouldBe` 4
          minOperations2033 [[1,5],[2,3]] 1 `shouldBe` 5
