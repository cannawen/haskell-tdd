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
    it "a single element repeated should return in (element count * succ interval) time" $ do
      time ["A", "A"] 0 `shouldBe` 2
      time ["A", "A"] 1 `shouldBe` 4
      time ["A", "A"] 2 `shouldBe` 6
