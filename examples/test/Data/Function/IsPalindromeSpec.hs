{-# LANGUAGE TemplateHaskell #-}

module Data.Function.IsPalindromeSpec where

import Control.Category.Execute.Haskell.WithDefinitions
import Control.Category.Execute.Haskell.WithImports
import Control.Category.Execute.JSON.WithDefinitions
import Control.Category.Execute.JSON.WithImports
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
import Test.Hspec                       hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_HSIsCorrectWithDefinitions ∷ String → Property
prop_HSIsCorrectWithDefinitions s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeViaGHCiWithDefinitions (isPalindrome :: HS String Bool) s
    pure $ answer === isPalindrome s

xprop_HSIsCorrectWithImports ∷ String → Property
xprop_HSIsCorrectWithImports s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeViaGHCiWithImports (isPalindrome :: HS String Bool) s
    pure $ answer === isPalindrome s

-- prop_JSIsCorrectWithDefinitions ∷ String → Property
-- prop_JSIsCorrectWithDefinitions s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
--     answer <- executeViaJSONWithDefinitions (isPalindrome :: JS String Bool) s
--     pure $ answer === isPalindrome s
-- 
-- prop_JSIsCorrectWithImports ∷ String → Property
-- prop_JSIsCorrectWithImports s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
--     answer <- executeViaJSONWithImports (isPalindrome :: JS String Bool) s
--     pure $ answer === isPalindrome s
-- 
-- prop_PHPIsCorrectWithDefinitions ∷ String → Property
-- prop_PHPIsCorrectWithDefinitions s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
--     answer <- executeViaJSONWithDefinitions (isPalindrome :: PHP String Bool) s
--     pure $ answer === isPalindrome s
-- 
-- prop_PHPIsCorrectWithImports ∷ String → Property
-- prop_PHPIsCorrectWithImports s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 . monadicIO $ do
--     answer <- executeViaJSONWithImports (isPalindrome :: PHP String Bool) s
--     pure $ answer === isPalindrome s

{-}

myInterpret = _

prop_ViaJSONIsCorrect :: String -> Property
prop_ViaJSONIsCorrect s = length s > 1 && all (`notElem` "$") s ==> withMaxSuccess 200 $
    (myInterpret <$> decode (encode (isPalindrome :: FreeFunc p String Bool)) <*> Just s) === Just (isPalindrome s)
-}

    {-}
    describe "JSON" $ do
        it "is correct" $
            decode (encode (isPalindrome :: FreeFunc p String Bool)) `shouldBe` Just (isPalindrome :: FreeFunc p String Bool)
    -}

return []
runTests = $quickCheckAll

spec ∷ Spec
spec = prop "all" . monadicIO . liftIO $ runTests
