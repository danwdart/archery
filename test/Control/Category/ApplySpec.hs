module Control.Category.ApplySpec (spec) where

import Control.Category.Apply
import Test.Hspec

spec ∷ Spec
spec = parallel $ pure () {-describe "apply" .
    describe "function arrow" .
        it "applies" $ app (+ 1) 1 `shouldBe` 2 -}
