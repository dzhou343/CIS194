module Week4Spec (spec) where

import Week4.HW04
import Test.Hspec

spec :: Spec
spec =
    describe "Week 4" $ do
        describe "equality" $ do
            it "should correctly check for equality" $
                P [1,1,2] == P [1,1,2] `shouldBe` True
            it "should correctly check for equality" $
                P [1,1,2] == P [1,1,2,0,0,0,0] `shouldBe` True
            it "should correctly check for equality" $
                P [1,1,2] == P [1,1,2,10,0,0] `shouldBe` False
        describe "show" $
            it "should print properly" $
            show (P [1,1,2]) `shouldBe` "2x^2 + x + 1"
        describe "plus" $ do
            it "should add properly" $
                P [5,0,1] + P [1,1,2] `shouldBe` P [6,1,3]
            it "should add properly2" $
                P [1,0,1] + P [1,1] `shouldBe` P [2,1,1]
            it "should add properly3" $
                P [] + P [1,1] `shouldBe` P [1,1]
        describe "times" $
            it "should multiply properly" $
            P [1,1,1] * P [2,2] `shouldBe` P [2,4,4,2]
        describe "negate" $
            it "should negate properly" $
            negate (P [1,2,3]) `shouldBe` P [-1, -2, -3]
        describe "fromInteger" $ do
            it "should convert integer to poly properly" $
                fromInteger 5 `shouldBe` P [5]
        describe "applyP" $ do
            it "should solve polynomials properly" $
                applyP (P [1,2,1]) 1 `shouldBe` 4
            it "should solve polynomials properly2" $
                applyP (P [1,2,1]) 2 `shouldBe` 9
        describe "deriv" $ do
            it "should differntiate a poly properly" $
                deriv (P [5,3,1]) `shouldBe` P [3,2]
        describe "nderiv" $ do
            it "should differntiate a poly n times properly" $
                nderiv 2 (P [5,3,1]) `shouldBe` P [2]
            