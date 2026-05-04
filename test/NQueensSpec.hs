module NQueensSpec (spec) where

import Test.Hspec
import Lib (listToArray)

import NQueens51 (
    calculateNQueens
    , isValidInDir)

spec :: Spec
spec = do
    describe "n Queens" $ do
        it "example 1" $ do
            calculateNQueens 4 `shouldBe` 2
        it "example 2" $ do
            calculateNQueens 1 `shouldBe` 1
        it "should for for board=9" $ do
            calculateNQueens 9 `shouldBe` 352
    describe "isValidInDir" $ do 
        describe "out of bounds" $ do
            it "should return True" $ do
                isValidInDir 4 [(0,0),(1,1),(2,2),(3,3)] (0,0) (-1,-1) `shouldBe` True
                isValidInDir 4 [(0,0),(1,1),(2,2),(3,3)] (0,0) (1,-1) `shouldBe` True
                isValidInDir 4 [(0,0),(1,1),(2,2),(3,3)] (0,0) (-1,1) `shouldBe` True
        describe "invalid" $ do
            it "should return False" $ do
                isValidInDir 4 [(0,0),(1,1),(2,2),(3,3)] (0,0) (1,1) `shouldBe` False
