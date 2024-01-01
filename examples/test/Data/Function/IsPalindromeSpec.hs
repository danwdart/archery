module Data.Function.IsPalindromeSpec where

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

prop_HSIsCorrect ∷ String → Property
prop_HSIsCorrect s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeViaGHCi (isPalindrome :: HS String Bool) s
    pure $ answer === isPalindrome s

prop_JSLambIsCorrect ∷ String → Property
prop_JSLambIsCorrect s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeViaJSON (isPalindrome :: JSLamb String Bool) s
    pure $ answer === isPalindrome s

prop_PHPLambIsCorrect ∷ String → Property
prop_PHPLambIsCorrect s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeViaJSON (isPalindrome :: PHPLamb String Bool) s
    pure $ answer === isPalindrome s

{-}

myInterpret = _

prop_ViaJSONIsCorrect :: String -> Property
prop_ViaJSONIsCorrect s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 $
    (myInterpret <$> decode (encode (isPalindrome :: FreeFunc p String Bool)) <*> Just s) === Just (isPalindrome s)
-}

spec ∷ Spec
spec = describe "isPalindrome" $ do
    describe "HS" $ do
        prop "is correct" prop_HSIsCorrect
    xdescribe "JSLamb" $ do
        prop "is correct" prop_JSLambIsCorrect
    xdescribe "PHPLamb" $ do
        prop "is correct" prop_PHPLambIsCorrect
    {-}
    describe "JSON" $ do
        it "is correct" $
            decode (encode (isPalindrome :: FreeFunc p String Bool)) `shouldBe` Just (isPalindrome :: FreeFunc p String Bool)
    -}
