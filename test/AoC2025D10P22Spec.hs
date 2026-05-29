module AoC2025D10P22Spec (spec) where

import Test.Hspec
import AoC2025D10P22 (
  Status(..)
  , parseJoltage
  , parseButtons
  , buttonsToJoltage
  , buttonDelta
  , findComboCount
  , machine2
  )
import Data.Array (listArray)

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
  describe "buttonsToJoltage" $ do
    it "turns button presses to the right joltage" $ do
      buttonsToJoltage [[1],[1]] 2 `shouldBe` [0,2]
      buttonsToJoltage [[0,1],[1]] 2 `shouldBe` [1,2]
      buttonsToJoltage [[0]] 1 `shouldBe` [1]
  describe "buttonDelta" $ do
    it "computes the per-press joltage contribution of a button" $ do
      buttonDelta 1 [0]   `shouldBe` listArray (0,0) [1]
      buttonDelta 2 [0,1] `shouldBe` listArray (0,1) [1,1]
      buttonDelta 2 [1,1] `shouldBe` listArray (0,1) [0,2]
  describe "findComboCount" $ do
    it "returns the total button presses for the first valid combo" $ do
      let arr xs = listArray (0, length xs - 1) xs
      let withDeltas size bs = map (\b -> (b, buttonDelta size b)) bs
      findComboCount (arr [0]) (withDeltas 1 [[0]])           `shouldBe` Just 0
      findComboCount (arr [1]) (withDeltas 1 [[0]])           `shouldBe` Just 1
      findComboCount (arr [2]) (withDeltas 1 [[0]])           `shouldBe` Just 2
      findComboCount (arr [1,2]) (withDeltas 2 [[0,1],[1],[0]]) `shouldBe` Just 2
  describe "machine2" $ do
    it "works?" $ do
      machine2 "[....] (3) (1,3) (2) (2,3) (0,2) (0,1) {0,0,0,1}" `shouldBe` 1
      machine2 "[....] (3) (1,3) (2) (2,3) (0,2) (0,1) {0,0,0,2}" `shouldBe` 2
      machine2 "[....] (3) (1,3) (1) {0,1,0,1}" `shouldBe` 1
      machine2 "[....] (3) (1,2,3) (1) {0,1,0,1}" `shouldBe` 2
      machine2 "[....] (3) (1,2,3) (1) {0,1,1,2}" `shouldBe` 2
      machine2 "[....] (3) (1,2,3) (1) {0,2,1,2}" `shouldBe` 3
      machine2 "[....] (3) (1,2,3) (1) {0,3,2,3}" `shouldBe` 4
      machine2 "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}" `shouldBe` 10
      machine2 "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}" `shouldBe` 12
      machine2 "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}" `shouldBe` 11
