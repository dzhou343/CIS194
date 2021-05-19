module Week2Spec (spec) where

import Week2.HW02 
import Test.Hspec

spec :: Spec
spec =
    describe "Week 2" $ do
        describe "exactMatches" $ do
            it "should return the number of exact matches between two codes." $
                exactMatches [Red, Blue, Orange] [Blue, Blue, Orange] `shouldBe` 2
        describe "countColors" $ do 
            it "should return the number of times a color appears in a Code." $
                countColors [Red, Red, Blue] `shouldBe` [2,0,1,0,0,0]
        describe "countColors' " $ do 
            it "should return the number of times a color appears in a Code." $
                countColors' [Red, Red, Blue] `shouldBe` [2,0,1,0,0,0]
        describe "matches'' " $ do 
            it "should return the number of total matches between the secret code and the guess" $
                matches'' [Red, Red, Red] [Red, Red, Red] `shouldBe` 3         
            it "should return no matches" $
                matches''  [Red] [Blue] `shouldBe` 0
        describe "getMove" $ do 
            it "should return a move towards the secret code." $
                getMove [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] `shouldBe` Move [Red, Orange, Orange, Blue] 1 2 
        describe "isConsistent" $ do 
            it "should check whether a given move is possible" $
                isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Yellow, Purple] `shouldBe` True
            it "should return false when given an inconsistent move" $
                isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Red, Purple] `shouldBe` False
        describe "filterCodes" $ do 
            it "should return a list of consistent moves" $
                filterCodes (Move [Red, Red, Blue, Green] 1 1) [[Red, Blue, Yellow, Purple]]  `shouldBe` [[Red, Blue, Yellow, Purple]]
            it "should return a list of consistent moves -- empty" $
                filterCodes (Move [Red, Red, Blue, Green] 1 1) [[Red, Blue, Red, Purple]]  `shouldBe` []
        describe "allCodes" $ do 
            it "should return all possible codes given a length" $
                allCodes 1 `shouldBe` [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]]
        describe "solve" $ do 
            it "should solve for a given code" $
                solve [Red, Blue] `shouldBe` [Move [Red, Red] 1 0, Move [Red, Green] 1 0, Move [Red, Blue] 2 0]
        