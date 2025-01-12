{-# LANGUAGE TemplateHaskell #-}

module Data.Function.IsPalindromeSpec (spec) where

import Control.Category.Execute.Haskell.Imports
import Control.Category.Execute.Haskell.Longhand
import Control.Category.Execute.Haskell.Shorthand
import Control.Category.Execute.JSON.Imports
import Control.Category.Execute.JSON.Longhand
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
import Test.Hspec                                 hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_HSGHCiIsCorrectLonghand ∷ String → Property
prop_HSGHCiIsCorrectLonghand s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeGHCiLonghand (isPalindrome :: HS String Bool) s
    pure $ answer === isPalindrome s

xprop_HSGHCiIsCorrectImports ∷ String → Property
xprop_HSGHCiIsCorrectImports s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeGHCiImports (isPalindrome :: HS String Bool) s
    pure $ answer === isPalindrome s

xprop_HSGHCiIsCorrectShorthand ∷ String → Property
xprop_HSGHCiIsCorrectShorthand s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeGHCiShorthand (isPalindrome :: HS String Bool) s
    pure $ answer === isPalindrome s

xprop_HSJSONIsCorrectLonghand ∷ String → Property
xprop_HSJSONIsCorrectLonghand s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONLonghand (isPalindrome :: HS String Bool) s
    pure $ answer === isPalindrome s

xprop_HSJSONIsCorrectImports ∷ String → Property
xprop_HSJSONIsCorrectImports s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONImports (isPalindrome :: HS String Bool) s
    pure $ answer === isPalindrome s

xprop_HSJSONIsCorrectShorthand ∷ String → Property
xprop_HSJSONIsCorrectShorthand s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONShorthand (isPalindrome :: HS String Bool) s
    pure $ answer === isPalindrome s

-- TODO bad control characters
xprop_JSIsCorrectLonghand ∷ String → Property
xprop_JSIsCorrectLonghand s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONLonghand (isPalindrome :: JS String Bool) s
    pure $ answer === isPalindrome s

xprop_JSIsCorrectImports ∷ String → Property
xprop_JSIsCorrectImports s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONImports (isPalindrome :: JS String Bool) s
    pure $ answer === isPalindrome s

xprop_JSIsCorrectShorthand ∷ String → Property
xprop_JSIsCorrectShorthand s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONShorthand (isPalindrome :: JS String Bool) s
    pure $ answer === isPalindrome s
--
-- prop_PHPIsCorrectLonghand ∷ String → Property
-- prop_PHPIsCorrectLonghand s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
--     answer <- executeJSONLonghand (isPalindrome :: PHP String Bool) s
--     pure $ answer === isPalindrome s
--
-- prop_PHPIsCorrectImports ∷ String → Property
-- prop_PHPIsCorrectImports s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
--     answer <- executeJSONImports (isPalindrome :: PHP String Bool) s
--     pure $ answer === isPalindrome s

{-}

myInterpret = _

xprop_ViaJSONIsCorrect :: String -> Property
xprop_ViaJSONIsCorrect s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 50 $
    (myInterpret <$> decode (encode (isPalindrome :: FreeFunc p String Bool)) <*> Just s) === Just (isPalindrome s)
-}

    {-}
    describe "JSON" $ do
        it "is correct" $
            decode (encode (isPalindrome :: FreeFunc p String Bool)) `shouldBe` Just (isPalindrome :: FreeFunc p String Bool)
    -}

pure []
runTests = $quickCheckAll

spec ∷ Spec
spec = prop "all" . monadicIO . liftIO $ runTests
