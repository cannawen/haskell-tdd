module AoC2025D10P2Spec (spec) where

import Test.Hspec
import AoC2025D10P1 (
  Status(..)
  , parseButtons
  , allCombinationOfButtons
  , applyButtons
  , machine)

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
  describe "machine" $ do
    it "passes the examples" $ do
      machine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}" `shouldBe` 2
      machine "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}" `shouldBe` 3
      machine "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}" `shouldBe` 2
