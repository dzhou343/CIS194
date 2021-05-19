module Week1Spec (spec) where

import Week1.HW01 
import Test.Hspec

spec :: Spec
spec =
    describe "Week 1" $ do
        describe "lastDigit" $ do
            it "should return last digit for a number" $
                lastDigit 15 `shouldBe` 5
        describe "doubleEveryOther" $
            it "some description ..." $
                doubleEveryOther [1,2,3,4] `shouldBe` [1,4,3,8]

         
