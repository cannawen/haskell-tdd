module AoC7Spec (spec) where

import Test.Hspec
import AoC7 (add)

spec :: Spec
spec = do 
    it "retuns" $
        add 1 1 `shouldBe` 2
