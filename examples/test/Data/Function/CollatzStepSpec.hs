{-# LANGUAGE TemplateHaskell #-}

module Data.Function.CollatzStepSpec (spec) where

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
import Data.Code.PHP
import Data.Code.TS
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
import Data.Prims
import Numeric.Natural
import Test.Hspec                                 hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

{- HLINT ignore "Use camelCase" -}

-- @TODO random functions

prop_HSGHCiIsCorrectLonghand ∷ Natural → Property
prop_HSGHCiIsCorrectLonghand i = i >= 0 ==> i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeGHCiLonghand (collatzStep :: HS Natural Natural) i
    pure $ answer === collatzStep i

xprop_HSGHCiIsCorrectImports ∷ Natural → Property
xprop_HSGHCiIsCorrectImports i = i >= 0 ==> i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeGHCiImports (collatzStep :: HS Natural Natural) i
    pure $ answer === collatzStep i

prop_HSGHCiIsCorrectShorthand ∷ Natural → Property
prop_HSGHCiIsCorrectShorthand i = i >= 0 ==> i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeGHCiShorthand (collatzStep :: HS Natural Natural) i
    pure $ answer === collatzStep i

-- xprop_HSJSONIsCorrectLonghand ∷ Natural → Property
-- xprop_HSJSONIsCorrectLonghand i = i >= 0 ==> i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
--     answer <- executeJSONLonghand (collatzStep :: HS Natural Natural) i
--     pure $ answer === collatzStep i
-- 
-- xprop_HSJSONIsCorrectImports ∷ Natural → Property
-- xprop_HSJSONIsCorrectImports i = i >= 0 ==> i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
--     answer <- executeJSONImports (collatzStep :: HS Natural Natural) i
--     pure $ answer === collatzStep i
-- 
-- xprop_HSJSONIsCorrectShorthand ∷ Natural → Property
-- xprop_HSJSONIsCorrectShorthand i = i >= 0 ==> i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
--     answer <- executeJSONShorthand (collatzStep :: HS Natural Natural) i
--     pure $ answer === collatzStep i

prop_JSIsCorrectLonghand ∷ Natural → Property
prop_JSIsCorrectLonghand i = i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONLonghand (collatzStep :: JS Natural Natural) i
    pure $ answer === collatzStep i

xprop_JSIsCorrectImports ∷ Natural → Property
xprop_JSIsCorrectImports i = i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONImports (collatzStep :: JS Natural Natural) i
    pure $ answer === collatzStep i

prop_JSIsCorrectShorthand ∷ Natural → Property
prop_JSIsCorrectShorthand i = i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONShorthand (collatzStep :: JS Natural Natural) i
    pure $ answer === collatzStep i

prop_TSIsCorrectLonghand ∷ Natural → Property
prop_TSIsCorrectLonghand i = i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONLonghand (collatzStep :: TS Natural Natural) i
    pure $ answer === collatzStep i

xprop_TSIsCorrectImports ∷ Natural → Property
xprop_TSIsCorrectImports i = i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONImports (collatzStep :: TS Natural Natural) i
    pure $ answer === collatzStep i

prop_TSIsCorrectShorthand ∷ Natural → Property
prop_TSIsCorrectShorthand i = i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONShorthand (collatzStep :: TS Natural Natural) i
    pure $ answer === collatzStep i

prop_PHPIsCorrectLonghand ∷ Natural → Property
prop_PHPIsCorrectLonghand i = i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONLonghand (collatzStep :: PHP Natural Natural) i
    pure $ answer === collatzStep i

xprop_PHPIsCorrectImports ∷ Natural → Property
xprop_PHPIsCorrectImports i = i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONImports (collatzStep :: PHP Natural Natural) i
    pure $ answer === collatzStep i

prop_PHPIsCorrectShorthand ∷ Natural → Property
prop_PHPIsCorrectShorthand i = i >= 1 ==> withMaxSuccess 50 . monadicIO $ do
    answer <- executeJSONShorthand (collatzStep :: PHP Natural Natural) i
    pure $ answer === collatzStep i

{-}
myNaturalerpret :: a
myNaturalerpret = _

prop_ViaJSONIsCorrect :: Natural -> Property
prop_ViaJSONIsCorrect i = i >= 1 ==> withMaxSuccess 50 $
    (myNaturalerpret <$> decode (encode (collatzStep :: FreeFunc p Natural Natural)) <*> Just i) === Just (collatzStep i)
-}


{-}
    describe "JSON" $ do
        it "is correct" $
            -- decode (encode (collatzStep :: FreeFunc p Natural Natural)) `shouldBe` Just (collatzStep :: FreeFunc p Natural Natural)
    -}

pure []
runTests = $quickCheckAll

-- just takes ages
spec ∷ Spec
spec = parallel . xprop "CollatzStep" . monadicIO . liftIO $ runTests
