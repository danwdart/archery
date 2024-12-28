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

-- @TODO random functions

xprop_HSIsCorrectLonghand ∷ Int → Property
xprop_HSIsCorrectLonghand i = i >= 0 ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeGHCiLonghand (collatzStep :: HS Int Int) i
    pure $ answer === collatzStep i

xprop_HSIsCorrectImports ∷ Int → Property
xprop_HSIsCorrectImports i = i >= 0 ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeGHCiImports (collatzStep :: HS Int Int) i
    pure $ answer === collatzStep i

xprop_HSIsCorrectShorthand ∷ Int → Property
xprop_HSIsCorrectShorthand i = i >= 0 ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeGHCiShorthand (collatzStep :: HS Int Int) i
    pure $ answer === collatzStep i

-- prop_JSIsCorrectLonghand ∷ Int → Property
-- prop_JSIsCorrectLonghand i = withMaxSuccess 200 . monadicIO $ do
--     answer <- executeJSONLonghand (collatzStep :: JS Int Int) i
--     pure $ answer === collatzStep i
--
-- prop_JSIsCorrectImports ∷ Int → Property
-- prop_JSIsCorrectImports i = withMaxSuccess 200 . monadicIO $ do
--     answer <- executeJSONImports (collatzStep :: JS Int Int) i
--     pure $ answer === collatzStep i
--
-- prop_PHPIsCorrectLonghand ∷ Int → Property
-- prop_PHPIsCorrectLonghand i = withMaxSuccess 200 . monadicIO $ do
--     answer <- executeJSONLonghand (collatzStep :: PHP Int Int) i
--     pure $ answer === collatzStep i
--
-- prop_PHPIsCorrectImports ∷ Int → Property
-- prop_PHPIsCorrectImports i = withMaxSuccess 200 . monadicIO $ do
--     answer <- executeJSONImports (collatzStep :: PHP Int Int) i
--     pure $ answer === collatzStep i

{-}
myInterpret :: a
myInterpret = _

xprop_ViaJSONIsCorrect :: Int -> Property
xprop_ViaJSONIsCorrect i = withMaxSuccess 200 $
    (myInterpret <$> decode (encode (collatzStep :: FreeFunc p Int Int)) <*> Just i) === Just (collatzStep i)
-}


{-}
    describe "JSON" $ do
        it "is correct" $
            -- decode (encode (collatzStep :: FreeFunc p Int Int)) `shouldBe` Just (collatzStep :: FreeFunc p Int Int)
    -}

pure []
runTests = $quickCheckAll

spec ∷ Spec
spec = prop "all" . monadicIO . liftIO $ runTests
