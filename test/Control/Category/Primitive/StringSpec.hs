module Control.Category.Primitive.StringSpec where

import Control.Arrow                       (Kleisli (..))
import Control.Category.Primitive.String
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec ∷ Spec
spec = describe "Abstract" $ do
    describe "function arrow" $ do
        describe "reverseString" .
            prop "reverses" $ \(x :: String) -> reverseString x === reverse x

    describe "Kleisli" $ do
        describe "reverseString" .
            prop "reverses" $ \(x :: String) -> monadicIO $ do
                a <- runKleisli reverseString x
                let b = reverse x
                pure $ a === b
