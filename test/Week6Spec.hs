module Week6Spec (spec) where

import Week6.HW06
import Test.Hspec

spec :: Spec
spec =
    describe "Week 6" $ do
        describe "fib1" $
            it "should correctly list fib nums" $
            take 5 fibs1 `shouldBe` [0,1,1,2,3]
        describe "fib2" $
            it "should correctly list fib nums" $
            take 5 fibs2 `shouldBe` [0,1,1,2,3]
        describe "sRepeat" $
            it "should correctly repeat" $
            take 5 (streamToList (sRepeat 5)) `shouldBe` [5,5,5,5,5]
        describe "sIterate" $
            it "should correctly iterate" $
            take 5 (streamToList (sIterate (+1) 5)) `shouldBe` [5,6,7,8,9]
        describe "sInterleave" $
            it "should correctly interleave" $
            take 5 (streamToList (sInterleave (sRepeat 1) (sRepeat 2))) `shouldBe` [1,2,1,2,1]
        describe "sTake" $
            it "should correctly take on a stream" $
            sTake 5 (sRepeat 5) `shouldBe` [5,5,5,5,5]
        describe "nats" $
            it "should correct list the naturals" $
            sTake 5 nats `shouldBe` [0,1,2,3,4]
        describe "ruler" $
            it "should correct list the ruler" $
            sTake 10 ruler `shouldBe` [0,1,0,2,0,1,0,3,0,1]
        describe "minMax" $
            it "should correctly find the min and max of a list" $
            minMax xs `shouldBe` Just (minimum xs, maximum xs)
                where 
                    xs = sTake 1000000 $ rand 7666532