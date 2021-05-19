module Week2Spec (spec) where

import Week2.HW02 
import Test.Hspec

spec :: Spec
spec =
    describe "Week 2" $ do
        describe "exactMatches" $ do
            it "should return the number of exact matches between two codes." $
                exactMatches [Red, Blue, Orange] [Blue, Blue, Orange] `shouldBe` 2

         
