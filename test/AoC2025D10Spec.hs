module AoC2025D10Spec (spec) where

import Test.Hspec
import AoC2025D10 (parseButtons, Status(..), allCombinationOfButtons, applyButtons)

spec :: Spec
spec = do 
  describe "parseButtons" $ do
    it "put the buttons in the right place" $ do
      parseButtons "[] (1) (2) {}" `shouldBe` [[1], [2]]
      parseButtons "[] (1,2) (3) {}" `shouldBe` [[1, 2], [3]]
  describe "allCombinationOfButtons" $ do
    it "collapse all buttons" $ do
      allCombinationOfButtons [[1]] `shouldBe` [[[1]],[]]
      allCombinationOfButtons [[1, 2]] `shouldBe` [[[1, 2]], []]
      allCombinationOfButtons [[1],[2]] `shouldBe` [[[1],[2]], [[1]], [[2]], []]
  describe "applyButtons" $ do
    it "apply all buttons" $ do
      applyButtons [Off, Off, Off] [[1],[2]] `shouldBe` [Off, On, On]
      applyButtons [Off, Off, Off] [[1, 2],[2]] `shouldBe` [Off, On, Off]
