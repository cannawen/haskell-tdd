module AoC2025D10P2Spec (spec) where

import Test.Hspec
import AoC2025D10P1 (
  parseButtons
  )

spec :: Spec
spec = do 
  describe "parseButtons" $ do
    it "put the buttons in the right place" $ do
      parseButtons "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}" `shouldBe` [[0,0,0,1],[0,1,0,1],[0,0,1,0],[0,0,1,1],[1,0,1,0],[1,1,0,0]]
  