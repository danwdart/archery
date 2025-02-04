module Control.Category.Primitive.StringSpec (spec) where

import Control.Arrow                     (Kleisli (..))
import Control.Category.Primitive.String
import Data.Text.Arbitrary                         (Text)
import Data.Text                         qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec ∷ Spec
spec = describe "Abstract" $ do
    describe "function arrow" $ do
        describe "reverseString" .
            prop "reverses" $ \(x :: Text) -> reverseString x === T.reverse x

    describe "Kleisli" $ do
        describe "reverseString" .
            prop "reverses" $ \(x :: Text) -> monadicIO $ do
                a <- runKleisli reverseString x
                let b = T.reverse x
                pure $ a === b
