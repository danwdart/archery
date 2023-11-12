module Control.Category.Primitive.BoolSpec where

import Control.Arrow                   (Kleisli (..))
import Control.Category.Primitive.Bool
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec ∷ Spec
spec = describe "Abstract" $ do
    describe "function arrow" $ do
        describe "eq" $ do
            it "is true" $
                eq (1, 1) `shouldBe` True
            it "is false" $
                eq (1, 2) `shouldBe` False
    describe "Kleisli" $ do
        describe "eq" $ do
            it "is true" $
                runKleisli eq (1, 1) `shouldReturn` True
            it "is false" $
                runKleisli eq (1, 2) `shouldReturn` False
