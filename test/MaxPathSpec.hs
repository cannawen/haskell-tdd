module MaxPathSpec (spec) where

import Test.Hspec
import MaxPath3742 (maxPath)

spec :: Spec
spec = do 
  it "retuns" $
    maxPath [[1]] 0 `shouldBe` -1
