module MaxScoreSpec (spec) where

import Test.Hspec
import Lib (listToArray)
import MaxScore3225 (maxScore, getScoreAt, getColScore, scoreGrid)

exampleArray = listToArray [[1,2,3],[4,5,6]]

spec :: Spec
spec = do
  it "all-zero grid scores 0" $ do
    maxScore [[0,0],[0,0]] `shouldBe` 0
  it "example 1" $ do
    maxScore [[0,0,0,0,0],[0,0,3,0,0],[0,1,0,0,0],[5,0,0,3,0],[0,0,0,0,2]] `shouldBe` 11
  it "example 2" $ do
    maxScore [[10,9,0,0,15],[7,1,0,8,0],[5,20,0,11,0],[0,0,0,1,2],[8,12,1,10,3]] `shouldBe` 94
  describe "score at index" $ do 
    it "should return the score at the index provided" $ do
      getScoreAt exampleArray 0 0 `shouldBe` 1
      getScoreAt exampleArray 1 2 `shouldBe` 6
    it "should return 0 if invalid index" $ do
      getScoreAt exampleArray 2 1 `shouldBe` 0
  describe "column score" $ do 
    it "should sum up left and right scores" $ do
      getColScore exampleArray 0 1 `shouldBe` 2
      getColScore exampleArray 1 1 `shouldBe` 7
  describe "scoring entire grid" $ do
    it "should count scores beside black spaces" $ do 
      scoreGrid exampleArray [] `shouldBe` 0
      scoreGrid exampleArray [(0,0)] `shouldBe` 2
      scoreGrid exampleArray [(0,0),(1,0)] `shouldBe` 7
      scoreGrid exampleArray [(0,0),(1,0),(0,2),(1,2)] `shouldBe` 7
      scoreGrid exampleArray [(0,1)] `shouldBe` 4
      scoreGrid exampleArray [(0,1),(1,1)] `shouldBe` 14