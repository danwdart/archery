

{-# LANGUAGE OverloadedStrings #-}

module Data.Function.ArrowySpec (spec) where

import Control.Category.Execute.Haskell.Imports
import Control.Category.Execute.Haskell.Longhand
import Control.Category.Execute.Haskell.Shorthand
import Control.Category.Execute.JSON.Imports
import Control.Category.Execute.JSON.Longhand
import Control.Category.Execute.JSON.Shorthand
import Data.Aeson
import Data.Code.Haskell
import Data.Code.JS
import Data.Code.PHP
import Data.Code.TS
import Data.Function.Arrowy
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Arrowy
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
import Data.Person
import Data.Prims
import Data.Text                                  (Text)
import Test.Hspec                                 hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import GHC.TypeLits (ErrorMessage(Text))


spec ∷ Spec
spec = do
    xdescribe "Arrowy" $ do
        describe "HS" $ do
            describe "GHCi" $ do
                describe "with Longhand" $ do
                    it "is correct" $ do
                        executeGHCiLonghand (arrowy :: HS Text Text) "abc123" `shouldReturn` "oh"
                        executeGHCiLonghand (arrowy :: HS Text Text) "pwd123" `shouldReturn` "oh"
                xdescribe "with imports" $ do
                    it "is correct" $ do
                        executeGHCiImports (arrowy :: HS Text Text) "abc123" `shouldReturn` "oh"
                        executeGHCiImports (arrowy :: HS Text Text) "pwd123" `shouldReturn` "oh"
                describe "with shorthand" $ do
                    it "is correct" $ do
                        executeGHCiShorthand (arrowy :: HS Text Text) "abc123" `shouldReturn` "jjj"
                        executeGHCiShorthand (arrowy :: HS Text Text) "pwd123" `shouldReturn` "ddd"
            -- xdescribe "JSON" $ do
            --     describe "with Longhand" $ do
            --         it "is correct" $ do
            --             executeJSONLonghand (arrowy :: HS Text Text) "abc123" `shouldReturn` "ww"
            --     xdescribe "with imports" $ do
            --         it "is correct" $ do
            --             executeJSONImports (arrowy :: HS Text Text) "abc123" `shouldReturn` "ww"
            --     describe "with shorthand" $ do
            --         it "is correct" $ do
            --             executeJSONShorthand (arrowy :: HS Text Text) "abc123" `shouldReturn` "ww"
        describe "JS" $ do
            describe "JSON" $ do
                describe "with Longhand" $ do
                    it "is correct" $ do
                        executeJSONLonghand (arrowy :: JS Text Text) "abc123" `shouldReturn` "ww"
                        executeJSONLonghand (arrowy :: JS Text Text) "pwd123" `shouldReturn` "ww"
                xdescribe "with imports" $ do
                    it "is correct" $ do
                        executeJSONImports (arrowy :: JS Text Text) "abc123" `shouldReturn` "ww"
                        executeJSONImports (arrowy :: JS Text Text) "pwd123" `shouldReturn` "ww"
                describe "with shorthand" $ do
                    it "is correct" $ do
                        executeJSONShorthand (arrowy :: JS Text Text) "abc123" `shouldReturn` "ww"
                        executeJSONShorthand (arrowy :: JS Text Text) "pwd123" `shouldReturn` "ww"
        describe "PHP" $ do
            describe "JSON" $ do
                describe "with Longhand" $ do
                    it "is correct" $ do
                        executeJSONLonghand (arrowy :: PHP Text Text) "abc123" `shouldReturn` "ww"
                        executeJSONLonghand (arrowy :: PHP Text Text) "pwd123" `shouldReturn` "ww"
                xdescribe "with imports" $ do
                    it "is correct" $ do
                        executeJSONImports (arrowy :: PHP Text Text) "abc123" `shouldReturn` "ww"
                        executeJSONImports (arrowy :: PHP Text Text) "pwd123" `shouldReturn` "ww"
                describe "with shorthand" $ do
                    it "is correct" $ do
                        executeJSONShorthand (arrowy :: PHP Text Text) "abc123" `shouldReturn` "ww"
                        executeJSONShorthand (arrowy :: PHP Text Text) "pwd123" `shouldReturn` "ww"
        describe "TS" $ do
            describe "JSON" $ do
                describe "with Longhand" $ do
                    it "is correct" $ do
                        executeJSONLonghand (arrowy :: TS Text Text) "abc123" `shouldReturn` "ww"
                        executeJSONLonghand (arrowy :: TS Text Text) "pwd123" `shouldReturn` "ww"
                xdescribe "with imports" $ do
                    it "is correct" $ do
                        executeJSONImports (arrowy :: TS Text Text) "abc123" `shouldReturn` "ww"
                        executeJSONImports (arrowy :: TS Text Text) "pwd123" `shouldReturn` "ww"
                describe "with shorthand" $ do
                    it "is correct" $ do
                        executeJSONShorthand (arrowy :: TS Text Text) "abc123" `shouldReturn` "ww"
                        executeJSONShorthand (arrowy :: TS Text Text) "pwd123" `shouldReturn` "ww"