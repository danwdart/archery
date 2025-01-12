module Data.Function.GreetSpec (spec) where

import Control.Category.Execute.Haskell.Imports
import Control.Category.Execute.Haskell.Longhand
import Control.Category.Execute.Haskell.Shorthand
import Control.Category.Execute.JSON.Imports
import Control.Category.Execute.JSON.Longhand
import Control.Category.Execute.JSON.Shorthand
import Data.Aeson
import Data.Code.Haskell
import Data.Code.JS
-- import Data.Code.PHP
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
import Data.Person
import Data.Prims
import Test.Hspec                                 hiding (runIO)
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
    --             executeGHCi (greetData :: HS Person String) myPerson `shouldReturn` greetData myPerson
    --         it "is correct" $
    --             executeGHCi (greetData :: HS Person String) myPerson `shouldReturn` greetData myPerson
    --     describe "JS" $ do
    --         it "is correct" $ do
    --             executeJSON (greetData :: JS Person String) myPerson `shouldReturn` greetData myPerson
    --     describe "PHP" $ do
    --         it "is correct" $ do
    --             executeJSON (greetData :: PHP Person String) myPerson `shouldReturn` greetData myPerson
    describe "greetTuple" $ do
        describe "HS" $ do
            describe "GHCi" $ do
                describe "with Longhand" $ do
                    it "is correct" $ do
                        executeGHCiLonghand (greetTuple :: HS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
                xdescribe "with imports" $ do
                    it "is correct" $ do
                        executeGHCiImports (greetTuple :: HS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
                xdescribe "with shorthand" $ do
                    it "is correct" $ do
                        executeGHCiShorthand (greetTuple :: HS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
            -- TODO bad control characters
            describe "JSON" $ do
                describe "with Longhand" $ do
                    xit "is correct" $ do
                        executeJSONLonghand (greetTuple :: HS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
                xdescribe "with imports" $ do
                    it "is correct" $ do
                        executeJSONImports (greetTuple :: HS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
                xdescribe "with shorthand" $ do
                    it "is correct" $ do
                        executeJSONShorthand (greetTuple :: HS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
        xdescribe "JS" $ do
            describe "with Longhand" $ do
                it "is correct" $ do
                    executeJSONLonghand (greetTuple :: JS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
            describe "with imports" $ do
                it "is correct" $ do
                    executeJSONImports (greetTuple :: JS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
            describe "with shorthand" $ do
                it "is correct" $ do
                    executeJSONShorthand (greetTuple :: JS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
        -- xdescribe "PHP" $ do
        --     describe "with Longhand" $ do
        --         it "is correct" $ do
        --             executeJSONLonghand (greetTuple :: PHP (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
        --     describe "with imports" $ do
        --         it "is correct" $ do
        --             executeJSONImports (greetTuple :: PHP (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
