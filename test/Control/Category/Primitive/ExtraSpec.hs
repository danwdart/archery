module Control.Category.Primitive.ExtraSpec (spec) where

import Control.Arrow                    (Kleisli (..))
import Control.Category.Primitive.Extra
import Test.Hspec

spec ∷ Spec
spec = describe "Extra" $ do
    describe "function arrow" $ do
        it "should cast int to string" $
            intToString 1 `shouldBe` "1"
        it "should concat two strings" $
            concatString ("hello ", "world") `shouldBe` "hello world"
        it "should produce a constant string" $
            constString "a" () `shouldBe` "a"
    describe "Kleisli" $ do
        it "should cast int to string" $ do
            runKleisli intToString 1 `shouldReturn` "1"
        it "should concat two strings" $
            runKleisli concatString ("hello ", "world") `shouldReturn` "hello world"
        it "should produce a constant string" $
            runKleisli (constString "a") () `shouldReturn` "a"
