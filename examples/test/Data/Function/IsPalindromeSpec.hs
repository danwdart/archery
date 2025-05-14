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
import Data.Text                                  (Text)
import Data.Text                                  qualified as T
import Test.Hspec                                 hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_HSGHCiIsCorrectLonghand ∷ Text → Property
prop_HSGHCiIsCorrectLonghand s = T.length s > 1 && T.all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeGHCiLonghand (isPalindrome :: HS Text Bool) s
    pure $ answer === isPalindrome s

xprop_HSGHCiIsCorrectImports ∷ Text → Property
xprop_HSGHCiIsCorrectImports s = T.length s > 1 && T.all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeGHCiImports (isPalindrome :: HS Text Bool) s
    pure $ answer === isPalindrome s

xprop_HSGHCiIsCorrectShorthand ∷ Text → Property
xprop_HSGHCiIsCorrectShorthand s = T.length s > 1 && T.all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeGHCiShorthand (isPalindrome :: HS Text Bool) s
    pure $ answer === isPalindrome s

xprop_HSJSONIsCorrectLonghand ∷ Text → Property
xprop_HSJSONIsCorrectLonghand s = T.length s > 1 && T.all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONLonghand (isPalindrome :: HS Text Bool) s
    pure $ answer === isPalindrome s

xprop_HSJSONIsCorrectImports ∷ Text → Property
xprop_HSJSONIsCorrectImports s = T.length s > 1 && T.all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONImports (isPalindrome :: HS Text Bool) s
    pure $ answer === isPalindrome s

xprop_HSJSONIsCorrectShorthand ∷ Text → Property
xprop_HSJSONIsCorrectShorthand s = T.length s > 1 && T.all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONShorthand (isPalindrome :: HS Text Bool) s
    pure $ answer === isPalindrome s

-- TODO bad control characters
xprop_JSIsCorrectLonghand ∷ Text → Property
xprop_JSIsCorrectLonghand s = T.length s > 1 && T.all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONLonghand (isPalindrome :: JS Text Bool) s
    pure $ answer === isPalindrome s

xprop_JSIsCorrectImports ∷ Text → Property
xprop_JSIsCorrectImports s = T.length s > 1 && T.all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONImports (isPalindrome :: JS Text Bool) s
    pure $ answer === isPalindrome s

xprop_JSIsCorrectShorthand ∷ Text → Property
xprop_JSIsCorrectShorthand s = T.length s > 1 && T.all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONShorthand (isPalindrome :: JS Text Bool) s
    pure $ answer === isPalindrome s
--
-- prop_PHPIsCorrectLonghand ∷ Text → Property
-- prop_PHPIsCorrectLonghand s = length s > 1 && T.all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
--     answer <- executeJSONLonghand (isPalindrome :: PHP Text Bool) s
--     pure $ answer === isPalindrome s
--
-- prop_PHPIsCorrectImports ∷ Text → Property
-- prop_PHPIsCorrectImports s = length s > 1 && T.all (`notElem` "$") s ==> withMaxSuccess 50 . monadicIO $ do
--     answer <- executeJSONImports (isPalindrome :: PHP Text Bool) s
--     pure $ answer === isPalindrome s

{-}

myInterpret = _

xprop_ViaJSONIsCorrect :: Text -> Property
xprop_ViaJSONIsCorrect s = length s > 1 && T.all (`notElem` "$") s ==> withMaxSuccess 50 $
    (myInterpret <$> decode (encode (isPalindrome :: FreeFunc p Text Bool)) <*> Just s) === Just (isPalindrome s)
-}

    {-}
    describe "JSON" $ do
        it "is correct" $
            decode (encode (isPalindrome :: FreeFunc p Text Bool)) `shouldBe` Just (isPalindrome :: FreeFunc p Text Bool)
    -}

pure []
runTests = $quickCheckAll

spec ∷ Spec
spec = prop "All" . monadicIO . liftIO $ runTests
