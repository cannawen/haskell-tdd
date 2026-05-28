module AoC2025D10P2Spec (spec) where

import Test.Hspec
import AoC2025D10P2 (
  Status(..)
  , parseJoltage
  , parseButtons
  , smashButtons
  , machine2
  )
import qualified Data.Set  as Set

spec :: Spec
spec = do 
  describe "parseJoltage" $ do
    it "put the buttons in the right place" $ do
      parseJoltage "[] (1) (2) {1,2,3}" `shouldBe` [1,2,3]
      parseJoltage "[] (1,2) (3) {2}" `shouldBe` [2]
  describe "parseButtons" $ do
    it "put the buttons in the right place" $ do
      parseButtons "[] (1) (2) {}" `shouldBe` [[1], [2]]
      parseButtons "[] (1,2) (3) {}" `shouldBe` [[1, 2], [3]]
  describe "smashButtons" $ do
    it "should be able to press buttons" $ do
      smashButtons [[1]] Set.empty [0,1] `shouldBe` [Set.fromList [([1],1)]]
      smashButtons [[0],[1]] Set.empty [0,1] `shouldBe` [Set.fromList [([1],1)]]
      smashButtons [[1],[0]] Set.empty [0,2] `shouldBe` [Set.fromList [([1],2)]]
  describe "machine2" $ do
    it "works?" $ do
      machine2 "[....] (3) (1,3) (2) (2,3) (0,2) (0,1) {0,0,0,1}" `shouldBe` 1
      machine2 "[....] (3) (1,3) (2) (2,3) (0,2) (0,1) {0,0,0,2}" `shouldBe` 1
      machine2 "[....] (3) (1,3) (1) {0,1,0,1}" `shouldBe` 1
      machine2 "[....] (3) (1,2,3) (1) {0,1,0,1}" `shouldBe` 2
      machine2 "[....] (3) (1,2,3) (1) {0,1,1,2}" `shouldBe` 2
      machine2 "[....] (3) (1,2,3) (1) {0,2,1,2}" `shouldBe` 3
      machine2 "[....] (3) (1,2,3) (1) {0,3,2,3}" `shouldBe` 4
      -- machine2 "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}" `shouldBe` 10
      -- machine2 "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}" `shouldBe` 12
      -- machine2 "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}" `shouldBe` 11
