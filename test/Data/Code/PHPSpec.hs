{-# LANGUAGE OverloadedStrings #-}

module Data.Code.PHPSpec (spec) where

import Control.Category
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.JSON.Imports
import Control.Category.Execute.JSON.Longhand
import Control.Category.Execute.JSON.Shorthand
import Control.Category.Numeric
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Curried
import Control.Category.Primitive.Extra
import Control.Category.Primitive.File
import Control.Category.Primitive.String
import Control.Category.Strong
import Control.Category.Symmetric
import Data.Code.PHP
import Data.Text                               (Text)
import Prelude                                 hiding (id, (.))
import Test.Hspec

{-}
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
-}

spec ∷ Spec
spec = parallel . describe "PHP" $ do
    describe "executeJSONLonghand" $ do
        it "returns a string" $
            executeJSONLonghand (id :: PHP Text Text) "1" `shouldReturn` "1"
        it "returns an int" $
            executeJSONLonghand (id :: PHP Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeJSONLonghand (id :: PHP Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeJSONLonghand (id :: PHP (Text, Int) (Text, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeJSONLonghand (id :: PHP (Either Text Int) (Either Text Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeJSONLonghand (id :: PHP (Either Text Int) (Either Text Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeJSONLonghand (id :: PHP (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeJSONLonghand (id :: PHP (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
        describe "bracket" .
            it "is idempotent" $
                executeJSONLonghand (bracket id :: PHP Text Text) "1" `shouldReturn` "1"
        describe "category" $ do
            it "composes" $
                executeJSONLonghand (id :: PHP Text Text) "1" `shouldReturn` "1"
        describe "cartesian" $ do
            it "copies" $
                executeJSONLonghand (copy :: PHP Text (Text, Text)) "1" `shouldReturn` ("1", "1")
            it "consumes" $
                executeJSONLonghand (consume :: PHP Text ()) "1" `shouldReturn` ()
            it "returns fst" $
                executeJSONLonghand (fst' :: PHP (Text, Int) Text) ("1", 1) `shouldReturn` "1"
            it "returns snd" $
                executeJSONLonghand (snd' :: PHP (Int, Text) Text) (1, "1") `shouldReturn` "1"
        describe "cocartesian" $ do
            it "injects Left" $ do
                executeJSONLonghand (injectL :: PHP Text (Either Text ())) "1" `shouldReturn` Left "1"
            it "injects Right" $ do
                executeJSONLonghand (injectR :: PHP Text (Either () Text)) "1" `shouldReturn` Right "1"
            describe "unify" $ do
                it "unifies Left" $
                    executeJSONLonghand (unify :: PHP (Either Text Text) Text) (Left "1") `shouldReturn` "1"
                it "unifies Right" $
                    executeJSONLonghand (unify :: PHP (Either Text Text) Text) (Right "1") `shouldReturn` "1"
            describe "tag" $ do
                it "tags Left" $
                    executeJSONLonghand (tag :: PHP (Bool, Text) (Either Text Text)) (False, "1") `shouldReturn` Left "1"
                it "tags Right" $
                    executeJSONLonghand (tag :: PHP (Bool, Text) (Either Text Text)) (True, "1") `shouldReturn` Right "1"
        describe "strong" $ do
            it "runs on first" $
                executeJSONLonghand (first' copy :: PHP (Text, Text) ((Text, Text), Text)) ("1", "2") `shouldReturn` (("1", "1"), "2")
            it "runs on second" $
                executeJSONLonghand (second' copy :: PHP (Text, Text) (Text, (Text, Text))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
        describe "choice" $ do
            describe "left'" $ do
                it "runs on left" $
                    executeJSONLonghand (left' copy :: PHP (Either Text Int) (Either (Text, Text) Int)) (Left "1") `shouldReturn` Left ("1", "1")
                it "doesn't run on right" $
                    executeJSONLonghand (left' copy :: PHP (Either Text Int) (Either (Text, Text) Int)) (Right 1) `shouldReturn` Right 1
            describe "right'" $ do
                it "doesn't run on left" $
                    executeJSONLonghand (right' copy :: PHP (Either Text Int) (Either Text (Int, Int))) (Left "1") `shouldReturn` Left "1"
                it "runs on right" $
                    executeJSONLonghand (right' copy :: PHP (Either Text Int) (Either Text (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
        describe "symmetric" $ do
            it "swaps" $
                executeJSONLonghand (swap :: PHP (Text, Int) (Int, Text)) ("1", 1) `shouldReturn` (1, "1")
            describe "swapEither" $ do
                it "swaps left" $
                    executeJSONLonghand (swapEither :: PHP (Either Text Text) (Either Text Text)) (Left "1") `shouldReturn` Right "1"
                it "swaps right" $
                    executeJSONLonghand (swapEither :: PHP (Either Text Text) (Either Text Text)) (Right "1") `shouldReturn` Left "1"
            it "reassocs" $
                executeJSONLonghand (reassoc :: PHP (Text, (Int, Bool)) ((Text, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
            xdescribe "reassocEither" $ do
                it "reassocs Left" $
                    executeJSONLonghand (reassocEither :: PHP (Either Text (Either Int Bool)) (Either (Either Text Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                it "reassocs Right (Left)" $
                    executeJSONLonghand (reassocEither :: PHP (Either Text (Either Int Bool)) (Either (Either Text Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                it "reassoc Right (Right)" $
                    executeJSONLonghand (reassocEither :: PHP (Either Text (Either Int Bool)) (Either (Either Text Int) Bool)) (Right (Right True)) `shouldReturn` Right True
        describe "primitive" $ do
            describe "eq" $ do
                it "equal" $
                    executeJSONLonghand (eq :: PHP (Text, Text) Bool) ("a", "a") `shouldReturn` True
                it "not equal" $
                    executeJSONLonghand (eq :: PHP (Text, Text) Bool) ("a", "b") `shouldReturn` False
            it "reverses string" $
                executeJSONLonghand (reverseString :: PHP Text Text) "abc" `shouldReturn` "cba"
        describe "primitiveconsole" $ pure ()
        describe "primitive extra" $ do
            it "converts int to string" $
                executeJSONLonghand (intToString :: PHP Int Text) 1 `shouldReturn` "1"
            it "concats string" $
                executeJSONLonghand (concatString :: PHP (Text, Text) Text) ("a", "b") `shouldReturn` "ab"
            it "returns const string" $ do
                executeJSONLonghand (constString "a" :: PHP () Text) () `shouldReturn` "a"
        describe "numeric" $ do
            it "returns const int" $ do
                executeJSONLonghand (num 1 :: PHP () Int) () `shouldReturn` 1
            it "negates" $ do
                executeJSONLonghand (negate' :: PHP Int Int) 1 `shouldReturn` (-1)
            it "adds" $ do
                executeJSONLonghand (add :: PHP (Int, Int) Int) (1, 2) `shouldReturn` 3
            it "mults" $ do
                executeJSONLonghand (mult :: PHP (Int, Int) Int) (2, 3) `shouldReturn` 6
            it "divs" $ do
                executeJSONLonghand (div' :: PHP (Int, Int) Int) (4, 2) `shouldReturn` 2
            it "mods" $ do
                executeJSONLonghand (mod' :: PHP (Int, Int) Int) (5, 2) `shouldReturn` 1
    xdescribe "executeJSONImports" $ do
        it "returns a string" $
            executeJSONImports (id :: PHP Text Text) "1" `shouldReturn` "1"
        it "returns an int" $
            executeJSONImports (id :: PHP Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeJSONImports (id :: PHP Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeJSONImports (id :: PHP (Text, Int) (Text, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeJSONImports (id :: PHP (Either Text Int) (Either Text Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeJSONImports (id :: PHP (Either Text Int) (Either Text Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeJSONImports (id :: PHP (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeJSONImports (id :: PHP (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
        describe "bracket" .
            it "is idempotent" $
                executeJSONImports (bracket id :: PHP Text Text) "1" `shouldReturn` "1"
        describe "category" $ do
            it "composes" $
                executeJSONImports (id :: PHP Text Text) "1" `shouldReturn` "1"
        describe "cartesian" $ do
            it "copies" $
                executeJSONImports (copy :: PHP Text (Text, Text)) "1" `shouldReturn` ("1", "1")
            it "consumes" $
                executeJSONImports (consume :: PHP Text ()) "1" `shouldReturn` ()
            it "returns fst" $
                executeJSONImports (fst' :: PHP (Text, Int) Text) ("1", 1) `shouldReturn` "1"
            it "returns snd" $
                executeJSONImports (snd' :: PHP (Int, Text) Text) (1, "1") `shouldReturn` "1"
        describe "cocartesian" $ do
            it "injects Left" $ do
                executeJSONImports (injectL :: PHP Text (Either Text ())) "1" `shouldReturn` Left "1"
            it "injects Right" $ do
                executeJSONImports (injectR :: PHP Text (Either () Text)) "1" `shouldReturn` Right "1"
            describe "unify" $ do
                it "unifies Left" $
                    executeJSONImports (unify :: PHP (Either Text Text) Text) (Left "1") `shouldReturn` "1"
                it "unifies Right" $
                    executeJSONImports (unify :: PHP (Either Text Text) Text) (Right "1") `shouldReturn` "1"
            describe "tag" $ do
                it "tags Left" $
                    executeJSONImports (tag :: PHP (Bool, Text) (Either Text Text)) (False, "1") `shouldReturn` Left "1"
                it "tags Right" $
                    executeJSONImports (tag :: PHP (Bool, Text) (Either Text Text)) (True, "1") `shouldReturn` Right "1"
        describe "strong" $ do
            it "runs on first" $
                executeJSONImports (first' copy :: PHP (Text, Text) ((Text, Text), Text)) ("1", "2") `shouldReturn` (("1", "1"), "2")
            it "runs on second" $
                executeJSONImports (second' copy :: PHP (Text, Text) (Text, (Text, Text))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
        describe "choice" $ do
            describe "left'" $ do
                it "runs on left" $
                    executeJSONImports (left' copy :: PHP (Either Text Int) (Either (Text, Text) Int)) (Left "1") `shouldReturn` Left ("1", "1")
                it "doesn't run on right" $
                    executeJSONImports (left' copy :: PHP (Either Text Int) (Either (Text, Text) Int)) (Right 1) `shouldReturn` Right 1
            describe "right'" $ do
                it "doesn't run on left" $
                    executeJSONImports (right' copy :: PHP (Either Text Int) (Either Text (Int, Int))) (Left "1") `shouldReturn` Left "1"
                it "runs on right" $
                    executeJSONImports (right' copy :: PHP (Either Text Int) (Either Text (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
        describe "symmetric" $ do
            it "swaps" $
                executeJSONImports (swap :: PHP (Text, Int) (Int, Text)) ("1", 1) `shouldReturn` (1, "1")
            describe "swapEither" $ do
                it "swaps left" $
                    executeJSONImports (swapEither :: PHP (Either Text Text) (Either Text Text)) (Left "1") `shouldReturn` Right "1"
                it "swaps right" $
                    executeJSONImports (swapEither :: PHP (Either Text Text) (Either Text Text)) (Right "1") `shouldReturn` Left "1"
            it "reassocs" $
                executeJSONImports (reassoc :: PHP (Text, (Int, Bool)) ((Text, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
            xdescribe "reassocEither" $ do
                it "reassocs Left" $
                    executeJSONImports (reassocEither :: PHP (Either Text (Either Int Bool)) (Either (Either Text Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                it "reassocs Right (Left)" $
                    executeJSONImports (reassocEither :: PHP (Either Text (Either Int Bool)) (Either (Either Text Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                it "reassoc Right (Right)" $
                    executeJSONImports (reassocEither :: PHP (Either Text (Either Int Bool)) (Either (Either Text Int) Bool)) (Right (Right True)) `shouldReturn` Right True
        describe "primitive" $ do
            describe "eq" $ do
                it "equal" $
                    executeJSONImports (eq :: PHP (Text, Text) Bool) ("a", "a") `shouldReturn` True
                it "not equal" $
                    executeJSONImports (eq :: PHP (Text, Text) Bool) ("a", "b") `shouldReturn` False
            it "reverses string" $
                executeJSONImports (reverseString :: PHP Text Text) "abc" `shouldReturn` "cba"
        describe "primitiveconsole" $ pure ()
        describe "primitive extra" $ do
            it "converts int to string" $
                executeJSONImports (intToString :: PHP Int Text) 1 `shouldReturn` "1"
            it "concats string" $
                executeJSONImports (concatString :: PHP (Text, Text) Text) ("a", "b") `shouldReturn` "ab"
            it "returns const string" $ do
                executeJSONImports (constString "a" :: PHP () Text) () `shouldReturn` "a"
        describe "numeric" $ do
            it "returns const int" $ do
                executeJSONImports (num 1 :: PHP () Int) () `shouldReturn` 1
            it "negates" $ do
                executeJSONImports (negate' :: PHP Int Int) 1 `shouldReturn` (-1)
            it "adds" $ do
                executeJSONImports (add :: PHP (Int, Int) Int) (1, 2) `shouldReturn` 3
            it "mults" $ do
                executeJSONImports (mult :: PHP (Int, Int) Int) (2, 3) `shouldReturn` 6
            it "divs" $ do
                executeJSONImports (div' :: PHP (Int, Int) Int) (4, 2) `shouldReturn` 2
            it "mods" $ do
                executeJSONImports (mod' :: PHP (Int, Int) Int) (5, 2) `shouldReturn` 1
    describe "executeJSONShorthand" $ do
        it "returns a string" $
            executeJSONShorthand (id :: PHP Text Text) "1" `shouldReturn` "1"
        it "returns an int" $
            executeJSONShorthand (id :: PHP Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeJSONShorthand (id :: PHP Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeJSONShorthand (id :: PHP (Text, Int) (Text, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeJSONShorthand (id :: PHP (Either Text Int) (Either Text Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeJSONShorthand (id :: PHP (Either Text Int) (Either Text Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeJSONShorthand (id :: PHP (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeJSONShorthand (id :: PHP (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
        describe "bracket" .
            it "is idempotent" $
                executeJSONShorthand (bracket id :: PHP Text Text) "1" `shouldReturn` "1"
        describe "category" $ do
            it "composes" $
                executeJSONShorthand (id :: PHP Text Text) "1" `shouldReturn` "1"
        describe "cartesian" $ do
            it "copies" $
                executeJSONShorthand (copy :: PHP Text (Text, Text)) "1" `shouldReturn` ("1", "1")
            it "consumes" $
                executeJSONShorthand (consume :: PHP Text ()) "1" `shouldReturn` ()
            it "returns fst" $
                executeJSONShorthand (fst' :: PHP (Text, Int) Text) ("1", 1) `shouldReturn` "1"
            it "returns snd" $
                executeJSONShorthand (snd' :: PHP (Int, Text) Text) (1, "1") `shouldReturn` "1"
        describe "cocartesian" $ do
            it "injects Left" $ do
                executeJSONShorthand (injectL :: PHP Text (Either Text ())) "1" `shouldReturn` Left "1"
            it "injects Right" $ do
                executeJSONShorthand (injectR :: PHP Text (Either () Text)) "1" `shouldReturn` Right "1"
            describe "unify" $ do
                it "unifies Left" $
                    executeJSONShorthand (unify :: PHP (Either Text Text) Text) (Left "1") `shouldReturn` "1"
                it "unifies Right" $
                    executeJSONShorthand (unify :: PHP (Either Text Text) Text) (Right "1") `shouldReturn` "1"
            describe "tag" $ do
                it "tags Left" $
                    executeJSONShorthand (tag :: PHP (Bool, Text) (Either Text Text)) (False, "1") `shouldReturn` Left "1"
                it "tags Right" $
                    executeJSONShorthand (tag :: PHP (Bool, Text) (Either Text Text)) (True, "1") `shouldReturn` Right "1"
        describe "strong" $ do
            it "runs on first" $
                executeJSONShorthand (first' copy :: PHP (Text, Text) ((Text, Text), Text)) ("1", "2") `shouldReturn` (("1", "1"), "2")
            it "runs on second" $
                executeJSONShorthand (second' copy :: PHP (Text, Text) (Text, (Text, Text))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
        describe "choice" $ do
            describe "left'" $ do
                it "runs on left" $
                    executeJSONShorthand (left' copy :: PHP (Either Text Int) (Either (Text, Text) Int)) (Left "1") `shouldReturn` Left ("1", "1")
                it "doesn't run on right" $
                    executeJSONShorthand (left' copy :: PHP (Either Text Int) (Either (Text, Text) Int)) (Right 1) `shouldReturn` Right 1
            describe "right'" $ do
                it "doesn't run on left" $
                    executeJSONShorthand (right' copy :: PHP (Either Text Int) (Either Text (Int, Int))) (Left "1") `shouldReturn` Left "1"
                it "runs on right" $
                    executeJSONShorthand (right' copy :: PHP (Either Text Int) (Either Text (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
        describe "symmetric" $ do
            it "swaps" $
                executeJSONShorthand (swap :: PHP (Text, Int) (Int, Text)) ("1", 1) `shouldReturn` (1, "1")
            describe "swapEither" $ do
                it "swaps left" $
                    executeJSONShorthand (swapEither :: PHP (Either Text Text) (Either Text Text)) (Left "1") `shouldReturn` Right "1"
                it "swaps right" $
                    executeJSONShorthand (swapEither :: PHP (Either Text Text) (Either Text Text)) (Right "1") `shouldReturn` Left "1"
            it "reassocs" $
                executeJSONShorthand (reassoc :: PHP (Text, (Int, Bool)) ((Text, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
            xdescribe "reassocEither" $ do
                it "reassocs Left" $
                    executeJSONShorthand (reassocEither :: PHP (Either Text (Either Int Bool)) (Either (Either Text Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                it "reassocs Right (Left)" $
                    executeJSONShorthand (reassocEither :: PHP (Either Text (Either Int Bool)) (Either (Either Text Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                it "reassoc Right (Right)" $
                    executeJSONShorthand (reassocEither :: PHP (Either Text (Either Int Bool)) (Either (Either Text Int) Bool)) (Right (Right True)) `shouldReturn` Right True
        describe "primitive" $ do
            describe "eq" $ do
                it "equal" $
                    executeJSONShorthand (eq :: PHP (Text, Text) Bool) ("a", "a") `shouldReturn` True
                it "not equal" $
                    executeJSONShorthand (eq :: PHP (Text, Text) Bool) ("a", "b") `shouldReturn` False
            it "reverses string" $
                executeJSONShorthand (reverseString :: PHP Text Text) "abc" `shouldReturn` "cba"
        describe "primitiveconsole" $ pure ()
        describe "primitive extra" $ do
            it "converts int to string" $
                executeJSONShorthand (intToString :: PHP Int Text) 1 `shouldReturn` "1"
            it "concats string" $
                executeJSONShorthand (concatString :: PHP (Text, Text) Text) ("a", "b") `shouldReturn` "ab"
            it "returns const string" $ do
                executeJSONShorthand (constString "a" :: PHP () Text) () `shouldReturn` "a"
        describe "numeric" $ do
            it "returns const int" $ do
                executeJSONShorthand (num 1 :: PHP () Int) () `shouldReturn` 1
            it "negates" $ do
                executeJSONShorthand (negate' :: PHP Int Int) 1 `shouldReturn` (-1)
            it "adds" $ do
                executeJSONShorthand (add :: PHP (Int, Int) Int) (1, 2) `shouldReturn` 3
            it "mults" $ do
                executeJSONShorthand (mult :: PHP (Int, Int) Int) (2, 3) `shouldReturn` 6
            it "divs" $ do
                executeJSONShorthand (div' :: PHP (Int, Int) Int) (4, 2) `shouldReturn` 2
            it "mods" $ do
                executeJSONShorthand (mod' :: PHP (Int, Int) Int) (5, 2) `shouldReturn` 1
