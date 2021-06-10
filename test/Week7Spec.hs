module Week7Spec (spec) where

import Week7.HW07
import Test.Hspec
import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V


spec :: Spec
spec =
    describe "Week 7" $ do
        describe "liftM" $
            it "lift up a function to a monad" $
            liftM (+1) (Just 5) `shouldBe` Just 6
        describe "swapV" $
            it "correctly swap" $
            swapV 0 1 (V.fromList [1, 2, 3]) `shouldBe` Just (V.fromList [2,1,3])
        describe "mapM" $
            it "correctly maps a monadic function" $
            Week7.HW07.mapM Just [1..10] `shouldBe` Just [1..10]
        describe "getElts" $
            it "correctly get index vals and put them in a maybe" $
            getElts [1,3] (V.fromList [0..9]) `shouldBe` Just [1, 3]
        describe "qSort" $
            it "correctly sort" $
            qsort (V.fromList [3,4,5,2,1,0,1]) `shouldBe` V.fromList [0,1,1,2,3,4,5]