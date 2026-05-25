module JumpGameVIISpec (spec) where

import Test.Hspec
import JumpGameVII1871 (canReachEnd)

spec :: Spec
spec = do 
  describe "min jumping" $ do
    it "should be able to jump to 0 index" $ do
      canReachEnd "10" 1 10 `shouldBe` True
      canReachEnd "1010" 1 10 `shouldBe` True
    it "should not be able to jump to 1 index" $ do
      canReachEnd "11" 1 10 `shouldBe` False
      canReachEnd "1011" 1 10 `shouldBe` False
