module Control.Category.Primitive.AbstractSpec where

import Control.Category.Primitive.Abstract
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Arrow (Kleisli(..))
import Test.QuickCheck.Monadic

spec :: Spec
spec = describe "Abstract" $ do
    describe "function arrow" $ do
        describe "eq" $ do
            it "is true" $
                eq (1, 1) `shouldBe` True
            it "is false" $
                eq (1, 2) `shouldBe` False
        describe "reverseString" .
            prop "reverses" $ \(x :: String) -> reverseString x === reverse x

    describe "Kleisli" $ do
        describe "eq" $ do
            it "is true" $
                runKleisli eq (1, 1) `shouldReturn` True
            it "is false" $
                runKleisli eq (1, 2) `shouldReturn` False
        describe "reverseString" .
            prop "reverses" $ \(x :: String) -> monadicIO $ do
                a <- runKleisli reverseString x
                let b = reverse x
                pure $ a === b