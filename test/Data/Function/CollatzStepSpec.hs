module Data.Function.CollatzStepSpec where

import Control.Category.Execute.Haskell
import Control.Category.Execute.JSON
import Data.Aeson
import Data.Code.Haskell
import Data.Code.JS.Lamb
import Data.Code.PHP.Lamb
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
import Data.Prims
import Test.Hspec                       hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- @TODO random functions

prop_HSIsCorrect ∷ Int → Property
prop_HSIsCorrect i = i >= 0 ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeViaGHCi (collatzStep :: HS Int Int) i
    pure $ answer === col

prop_JSLambIsCorrect ∷ Int → Property
prop_JSLambIsCorrect i = withMaxSuccess 200 . monadicIO $ do
    answer <- executeViaJSON (collatzStep :: JSLamb Int Int) i
    pure $ answer === collatzStep i

prop_PHPLambIsCorrect ∷ Int → Property
prop_PHPLambIsCorrect i = withMaxSuccess 200 . monadicIO $ do
    answer <- executeViaJSON (collatzStep :: PHPLamb Int Int) i
    pure $ answer === collatzStep i

{-}
myInterpret :: a
myInterpret = _

prop_ViaJSONIsCorrect :: Int -> Property
prop_ViaJSONIsCorrect i = withMaxSuccess 200 $
    (myInterpret <$> decode (encode (collatzStep :: FreeFunc p Int Int)) <*> Just i) === Just (collatzStep i)
-}


spec ∷ Spec
spec = describe "collatzStep" $ do
    describe "HS" $ do
        prop "is correct" prop_HSIsCorrect
    xdescribe "JSLamb" $ do
        prop "is correct" prop_JSLambIsCorrect
    xdescribe "PHPLamb" $ do
        prop "is correct" prop_PHPLambIsCorrect
    {-
    describe "JSON" $ do
        it "is correct" $
            -- decode (encode (collatzStep :: FreeFunc p Int Int)) `shouldBe` Just (collatzStep :: FreeFunc p Int Int)
    -}
