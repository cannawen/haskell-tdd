module JumpGameVIISpec (spec) where

import Test.Hspec
import JumpGameVII1871 (canReachEnd)

spec :: Spec
spec = do 
  it "works" $ do
    canReachEnd "10" 1 1 `shouldBe` True