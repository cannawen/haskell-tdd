module MaxPathSpec (spec) where

import Test.Hspec
import MaxPath3742 (maxPath)

spec :: Spec
spec = do 
  it "works" $ do
    maxPath [[0, 1],[1, 2],[0,0]] 0 `shouldBe` -1
    maxPath [[0, 1],[1, 2],[0,0]] 1 `shouldBe` 1
    maxPath [[0, 1],[1, 2],[0,0]] 2 `shouldBe` 3 
    maxPath [[0, 1],[2, 0]] 1 `shouldBe` 2
    maxPath [[0, 1],[1, 2]] 1 `shouldBe` -1
