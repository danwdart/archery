{-# LANGUAGE TemplateHaskell #-}

module Data.Function.CollatzStepSpec where

import Control.Category.Execute.Haskell.Longhand
import Control.Category.Execute.Haskell.Imports
import Control.Category.Execute.Haskell.Shorthand
import Control.Category.Execute.JSON.Longhand
import Control.Category.Execute.JSON.Imports
import Control.Category.Execute.JSON.Shorthand
import Control.Monad.IO.Class
import Data.Aeson
import Data.Code.Haskell
import Data.Code.JS
-- import Data.Code.PHP
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
import Data.Prims
import Numeric.Natural
import Test.Hspec                                       hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural

-- @TODO random functions

prop_HSGHCiIsCorrectLonghand ∷ Natural → Property
prop_HSGHCiIsCorrectLonghand i = i >= 0 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeGHCiLonghand (collatzStep :: HS Natural Natural) i
    pure $ answer === collatzStep i

xprop_HSGHCiIsCorrectImports ∷ Natural → Property
xprop_HSGHCiIsCorrectImports i = i >= 0 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeGHCiImports (collatzStep :: HS Natural Natural) i
    pure $ answer === collatzStep i

xprop_HSGHCiIsCorrectShorthand ∷ Natural → Property
xprop_HSGHCiIsCorrectShorthand i = i >= 0 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeGHCiShorthand (collatzStep :: HS Natural Natural) i
    pure $ answer === collatzStep i

xprop_HSJSONIsCorrectLonghand ∷ Natural → Property
xprop_HSJSONIsCorrectLonghand i = i >= 0 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONLonghand (collatzStep :: HS Natural Natural) i
    pure $ answer === collatzStep i

xprop_HSJSONIsCorrectImports ∷ Natural → Property
xprop_HSJSONIsCorrectImports i = i >= 0 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONImports (collatzStep :: HS Natural Natural) i
    pure $ answer === collatzStep i

xprop_HSJSONIsCorrectShorthand ∷ Natural → Property
xprop_HSJSONIsCorrectShorthand i = i >= 0 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONShorthand (collatzStep :: HS Natural Natural) i
    pure $ answer === collatzStep i

prop_JSIsCorrectLonghand ∷ Natural → Property
prop_JSIsCorrectLonghand i = withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONLonghand (collatzStep :: JS Natural Natural) i
    pure $ answer === collatzStep i

xprop_JSIsCorrectImports ∷ Natural → Property
xprop_JSIsCorrectImports i = withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONImports (collatzStep :: JS Natural Natural) i
    pure $ answer === collatzStep i

prop_JSIsCorrectShorthand ∷ Natural → Property
prop_JSIsCorrectShorthand i = withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONShorthand (collatzStep :: JS Natural Natural) i
    pure $ answer === collatzStep i
--
-- prop_PHPIsCorrectLonghand ∷ Natural → Property
-- prop_PHPIsCorrectLonghand i = withMaxSuccess 50 . monadicIO $ do
--     answer <- executeJSONLonghand (collatzStep :: PHP Natural Natural) i
--     pure $ answer === collatzStep i
--
-- prop_PHPIsCorrectImports ∷ Natural → Property
-- prop_PHPIsCorrectImports i = withMaxSuccess 50 . monadicIO $ do
--     answer <- executeJSONImports (collatzStep :: PHP Natural Natural) i
--     pure $ answer === collatzStep i

{-}
myNaturalerpret :: a
myNaturalerpret = _

xprop_ViaJSONIsCorrect :: Natural -> Property
xprop_ViaJSONIsCorrect i = withMaxSuccess 50 $
    (myNaturalerpret <$> decode (encode (collatzStep :: FreeFunc p Natural Natural)) <*> Just i) === Just (collatzStep i)
-}


{-}
    describe "JSON" $ do
        it "is correct" $
            -- decode (encode (collatzStep :: FreeFunc p Natural Natural)) `shouldBe` Just (collatzStep :: FreeFunc p Natural Natural)
    -}

pure []
runTests = $quickCheckAll

spec ∷ Spec
spec = prop "all" . monadicIO . liftIO $ runTests
