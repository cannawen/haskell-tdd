module MinOpSpec (spec) where

import Test.Hspec
import MinOp2023 (main)

spec :: Spec
spec = do
  describe "main" $ do
    describe "invalid input" $ do
      describe "empty input" $ do
        it "returns -1" $ do
          main [[]] 1 `shouldBe` -1
      describe "unreachable input" $ do
        it "returns -1" $ do
          main [[5,3]] 3 `shouldBe` -1
          main [[1,2],[3,4]] 2 `shouldBe` -1
      describe "no steps" $ do
        it "returns -1" $ do
          main [[5,3]] 0 `shouldBe` -1

    describe "valid input" $ do
      describe "single input" $ do
        it "returns 0" $ do
          main [[0]] 1 `shouldBe` 0
          main [[1]] 1 `shouldBe` 0
          main [[1]] 0 `shouldBe` 0
      describe "one step away" $ do
        it "returns 1" $ do
          main [[4,2]] 2 `shouldBe` 1
          main [[3,5]] 2 `shouldBe` 1
      describe "2D array should be flattened" $ do
        it "returns accurately" $ do
          main [[2,4],[6,8]] 2 `shouldBe` 4
          main [[1,5],[2,3]] 1 `shouldBe` 5
