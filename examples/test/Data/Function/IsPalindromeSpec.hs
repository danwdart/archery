{-# LANGUAGE TemplateHaskell #-}

module Data.Function.IsPalindromeSpec where

import Control.Category.Execute.Haskell.Longhand
import Control.Category.Execute.Haskell.Imports
import Control.Category.Execute.Haskell.Shorthand
import Control.Category.Execute.JSON.Longhand
import Control.Category.Execute.JSON.Imports
import Control.Category.Execute.JSON.Shorthand
import Control.Monad.IO.Class
import Data.Aeson
import Data.Code.Haskell
-- import Data.Code.JS
-- import Data.Code.PHP
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
import Data.Prims
import Test.Hspec                                       hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_HSIsCorrectLonghand ∷ String → Property
prop_HSIsCorrectLonghand s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeGHCiLonghand (isPalindrome :: HS String Bool) s
    pure $ answer === isPalindrome s

xprop_HSIsCorrectImports ∷ String → Property
xprop_HSIsCorrectImports s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeGHCiImports (isPalindrome :: HS String Bool) s
    pure $ answer === isPalindrome s

xprop_HSIsCorrectShorthand ∷ String → Property
xprop_HSIsCorrectShorthand s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeGHCiShorthand (isPalindrome :: HS String Bool) s
    pure $ answer === isPalindrome s

-- prop_JSIsCorrectLonghand ∷ String → Property
-- prop_JSIsCorrectLonghand s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
--     answer <- executeJSONLonghand (isPalindrome :: JS String Bool) s
--     pure $ answer === isPalindrome s
--
-- prop_JSIsCorrectImports ∷ String → Property
-- prop_JSIsCorrectImports s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
--     answer <- executeJSONImports (isPalindrome :: JS String Bool) s
--     pure $ answer === isPalindrome s
--
-- prop_PHPIsCorrectLonghand ∷ String → Property
-- prop_PHPIsCorrectLonghand s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
--     answer <- executeJSONLonghand (isPalindrome :: PHP String Bool) s
--     pure $ answer === isPalindrome s
--
-- prop_PHPIsCorrectImports ∷ String → Property
-- prop_PHPIsCorrectImports s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
--     answer <- executeJSONImports (isPalindrome :: PHP String Bool) s
--     pure $ answer === isPalindrome s

{-}

myInterpret = _

xprop_ViaJSONIsCorrect :: String -> Property
xprop_ViaJSONIsCorrect s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 $
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
