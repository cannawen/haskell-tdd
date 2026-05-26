module JumpGameVIISpec (spec) where

import Test.Hspec
import JumpGameVII1871 (canReachEnd)

spec :: Spec
spec = do 
  describe "min jumping" $ do
    it "should be able to jump to 0 index" $ do
      canReachEnd "010" 2 2 `shouldBe` True
      canReachEnd "01010" 2 2 `shouldBe` True
    it "should not be able to jump to 1 index" $ do
      canReachEnd "011" 2 2 `shouldBe` False
      canReachEnd "01011" 2 2 `shouldBe` False
      canReachEnd "010100" 2 2 `shouldBe` False
  describe "max jumping" $ do
    it "should be able to jump to 0 index" $ do
      canReachEnd "0110" 2 3 `shouldBe` True
      canReachEnd "0110110" 2 3 `shouldBe` True
    it "should not be able to jump to 1 index" $ do
      canReachEnd "0111" 2 3 `shouldBe` False
      canReachEnd "0110111" 2 3 `shouldBe` False
      canReachEnd "01101100" 2 3 `shouldBe` False
  describe "given test cases" $ do
    it "should work" $ do
      canReachEnd "011010" 2 3 `shouldBe` True
      canReachEnd "01101110" 2 3 `shouldBe` False
      canReachEnd "000000000" 6 7 `shouldBe` False
