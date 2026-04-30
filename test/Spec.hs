module Main (main) where

import Test.Hspec
import Lib (minOperations2033)

main :: IO ()
main = hspec $ do
  describe "minOperations2033" $ do
    it "returns -1 on invalid input" $
      minOperations2033 [[]] 1 `shouldBe` (-1)

