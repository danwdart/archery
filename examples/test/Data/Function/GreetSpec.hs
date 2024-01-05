module Data.Function.GreetSpec where

import Control.Category.Execute.Haskell.WithDefinitions
import Control.Category.Execute.Haskell.WithImports
import Control.Category.Execute.JSON.WithDefinitions
import Control.Category.Execute.JSON.WithImports
import Data.Aeson
import Data.Code.Haskell
-- import Data.Code.JS
-- import Data.Code.PHP
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
import Data.Person
import Data.Prims
import Test.Hspec                       hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

myPerson ∷ Person
myPerson = Person "Dan" 32

myTuple ∷ (String, Int)
myTuple = ("Dan", 32)

spec ∷ Spec
spec = do
    -- xdescribe "greetData" $ do
    --     describe "HS" $ do
    --         it "is correct" $
    --             executeViaGHCi (greetData :: HS Person String) myPerson `shouldReturn` greetData myPerson
    --         it "is correct" $
    --             executeViaGHCi (greetData :: HS Person String) myPerson `shouldReturn` greetData myPerson
    --     describe "JS" $ do
    --         it "is correct" $ do
    --             executeViaJSON (greetData :: JS Person String) myPerson `shouldReturn` greetData myPerson
    --     describe "PHP" $ do
    --         it "is correct" $ do
    --             executeViaJSON (greetData :: PHP Person String) myPerson `shouldReturn` greetData myPerson
    describe "greetTuple" $ do
        describe "HS" $ do
            describe "with definitions" $ do
                it "is correct" $ do
                    executeViaGHCiWithDefinitions (greetTuple :: HS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
            xdescribe "with imports" $ do
                it "is correct" $ do
                    executeViaGHCiWithImports (greetTuple :: HS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
        -- xdescribe "JS" $ do
        --     describe "with definitions" $ do
        --         it "is correct" $ do
        --             executeViaJSONWithDefinitions (greetTuple :: JS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
        --     describe "with imports" $ do
        --         it "is correct" $ do
        --             executeViaJSONWithImports (greetTuple :: JS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
        -- xdescribe "PHP" $ do
        --     describe "with definitions" $ do
        --         it "is correct" $ do
        --             executeViaJSONWithDefinitions (greetTuple :: PHP (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
        --     describe "with imports" $ do
        --         it "is correct" $ do
        --             executeViaJSONWithImports (greetTuple :: PHP (String, Int) String) myTuple `shouldReturn` greetTuple myTuple