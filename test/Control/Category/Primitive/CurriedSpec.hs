module Control.Category.Primitive.CurriedSpec where

import Control.Arrow                      (Kleisli (..))
import Control.Category.Primitive.Curried
import Test.Hspec


spec ∷ Spec
spec = describe "curried" $ do
    describe "function arrow" $ do
        it "should equal" $
            eqCurried 1 1 `shouldBe` True
        it "should not equal" $
            eqCurried 1 2 `shouldBe` False
    {-}
    describe "Kleisli" $ do
        it "should equal, monadically" $ do
            runKleisli (runKleisli (eqCurried :: Kleisli IO Int (Kleisli IO Int Bool)) 1) 1 `shouldReturn` True
        it "should not equal, monadically" $ do
            runKleisli (runKleisli (eqCurried :: Kleisli IO Int (Kleisli IO Int Bool)) 1) 2 `shouldReturn` False
    -}
