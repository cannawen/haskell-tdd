module AoC2025D10Spec (spec) where

import Test.Hspec
import AoC2025D10 (machine)

spec :: Spec
spec = do 
  describe "machine" $ do
    it "should" $ do
      machine "[] (1) (2) {}" `shouldBe` True