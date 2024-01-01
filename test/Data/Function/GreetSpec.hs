module Data.Function.GreetSpec where

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
    --     describe "JSLamb" $ do
    --         it "is correct" $ do
    --             executeViaJSON (greetData :: JSLamb Person String) myPerson `shouldReturn` greetData myPerson
    --     describe "PHPLamb" $ do
    --         it "is correct" $ do
    --             executeViaJSON (greetData :: PHPLamb Person String) myPerson `shouldReturn` greetData myPerson
    describe "greetTuple" $ do
        describe "HS" $ do
            it "is correct" $ do
                executeViaGHCi (greetTuple :: HS (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
        xdescribe "JSLamb" $ do
            it "is correct" $ do
                executeViaJSON (greetTuple :: JSLamb (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
        xdescribe "PHPLamb" $ do
            it "is correct" $ do
                executeViaJSON (greetTuple :: PHPLamb (String, Int) String) myTuple `shouldReturn` greetTuple myTuple
