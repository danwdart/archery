{-# LANGUAGE TemplateHaskell #-}

module Data.Function.CollatzStepSpec where

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

-- @TODO random functions

prop_HSIsCorrectWithDefinitions ∷ Int → Property
prop_HSIsCorrectWithDefinitions i = i >= 0 ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeViaGHCiWithDefinitions (collatzStep :: HS Int Int) i
    pure $ answer === collatzStep i

xprop_HSIsCorrectWithImports ∷ Int → Property
xprop_HSIsCorrectWithImports i = i >= 0 ==> withMaxSuccess 200 . monadicIO $ do
    answer <- executeViaGHCiWithImports (collatzStep :: HS Int Int) i
    pure $ answer === collatzStep i

-- prop_JSIsCorrectWithDefinitions ∷ Int → Property
-- prop_JSIsCorrectWithDefinitions i = withMaxSuccess 200 . monadicIO $ do
--     answer <- executeViaJSONWithDefinitions (collatzStep :: JS Int Int) i
--     pure $ answer === collatzStep i
-- 
-- prop_JSIsCorrectWithImports ∷ Int → Property
-- prop_JSIsCorrectWithImports i = withMaxSuccess 200 . monadicIO $ do
--     answer <- executeViaJSONWithImports (collatzStep :: JS Int Int) i
--     pure $ answer === collatzStep i
-- 
-- prop_PHPIsCorrectWithDefinitions ∷ Int → Property
-- prop_PHPIsCorrectWithDefinitions i = withMaxSuccess 200 . monadicIO $ do
--     answer <- executeViaJSONWithDefinitions (collatzStep :: PHP Int Int) i
--     pure $ answer === collatzStep i
-- 
-- prop_PHPIsCorrectWithImports ∷ Int → Property
-- prop_PHPIsCorrectWithImports i = withMaxSuccess 200 . monadicIO $ do
--     answer <- executeViaJSONWithImports (collatzStep :: PHP Int Int) i
--     pure $ answer === collatzStep i

{-}
myInterpret :: a
myInterpret = _

prop_ViaJSONIsCorrect :: Int -> Property
prop_ViaJSONIsCorrect i = withMaxSuccess 200 $
    (myInterpret <$> decode (encode (collatzStep :: FreeFunc p Int Int)) <*> Just i) === Just (collatzStep i)
-}


{-}
    describe "JSON" $ do
        it "is correct" $
            -- decode (encode (collatzStep :: FreeFunc p Int Int)) `shouldBe` Just (collatzStep :: FreeFunc p Int Int)
    -}

return []
runTests = $quickCheckAll

spec ∷ Spec
spec = prop "all" . monadicIO . liftIO $ runTests
