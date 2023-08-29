module Control.Category.CartesianSpec where

import Control.Arrow              (Kleisli (..))
import Control.Category.Cartesian
import Test.Hspec

spec ∷ Spec
spec = describe "Cartesian" $ do
    describe "function arrow" $ do
        describe "copy" .
            it "copies" $
                copy 1 `shouldBe` (1, 1)
        describe "consume" .
            it "consumes" $
                consume 1 `shouldBe` ()
        describe "fst'" .
            it "gives fst" $
                fst' (1, 2) `shouldBe` 1
        describe "snd'" .
            it "gives snd" $
                snd' (1, 2) `shouldBe` 2
    describe "Kleisli" $ do
        describe "copy" .
            it "copies" $
                runKleisli copy 1 `shouldReturn` (1, 1)
        describe "consume" .
            it "consumes" $
                runKleisli consume 1 `shouldReturn` ()
        describe "fst'" .
            it "gives fst" $
                runKleisli fst' (1, 2) `shouldReturn` 1
        describe "snd'" .
            it "gives snd" $
                runKleisli snd' (1, 2) `shouldReturn` 2
