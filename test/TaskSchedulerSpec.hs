module TaskSchedulerSpec (spec) where

import Test.Hspec
import TaskScheduler621 (time)

spec :: Spec
spec = do 
  describe "a single element type" $ do
    it "a single element should return in 1 time" $ do
      time ["A"] 0 `shouldBe` 1
      time ["A"] 1 `shouldBe` 1
      time ["A"] 2 `shouldBe` 1
    it "a single element repeated should return in element count * ((element count - 1) interval) time" $ do
      time ["A", "A", "A"] 0 `shouldBe` 3
      time ["A", "A", "A"] 1 `shouldBe` 5
      time ["A", "A", "A"] 2 `shouldBe` 7
  describe "two different elements" $ do 
    it "should place them side by side" $ do
      time ["A", "B"] 0 `shouldBe` 2
      time ["A", "B"] 1 `shouldBe` 2
      time ["A", "B"] 2 `shouldBe` 2
  describe "test cases" $ do 
    it "works" $ do 
      time ["A","A","A","B","B","B"] 2 `shouldBe` 8
      time ["A","C","A","B","D","B"] 1 `shouldBe` 6
      time ["A","A","A", "B","B","B"] 3 `shouldBe` 10
