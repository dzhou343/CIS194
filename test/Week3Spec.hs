module Week3Spec (spec) where

import Week3.HW03 
import Test.Hspec

spec :: Spec
spec =
    describe "Week 3" $ do
        describe "extend" $ do
            it "should extend a state to include new assignments" $
                let st = extend empty "A" 5 in st "A" `shouldBe` 5
        describe "evalE" $ do
            it "should, given a state and expression, reduce it to an integer value" $
                evalE empty (Op (Val 1) Eql (Val 2)) `shouldBe` 0
        describe "desugar" $ do
            it "should, given a statement, desugar it" $
                desugar (Incr "A") `shouldBe` DAssign "A" (Op (Var "A") Plus (Val 1))
        describe "evalSimple" $ do
            it "should evaluate desugared statements, returning a new state" $
                let s = evalSimple empty (DAssign "A" (Val 10)) in s "A" `shouldBe` 10
        describe "run" $ do
            it "should evaluate sugary statements" $
                let s = run empty (Assign "A" (Val 10)) in s "A" `shouldBe` 10 