module Data.Code.HaskellSpec where

import Control.Category
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.Haskell.Longhand
import Control.Category.Execute.Haskell.Imports
import Control.Category.Numeric
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Curried
import Control.Category.Primitive.Extra
import Control.Category.Primitive.File
import Control.Category.Primitive.String
import Control.Category.Strong
import Control.Category.Symmetric
import Data.Code.Haskell
import Prelude                                          hiding (id, (.))
import Test.Hspec                                       hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec ∷ Spec
spec = describe "HS" $ do
    describe "with Longhand" $ do
        -- describe "bracket" . it "is idempotent" $ (do
        --         executeGHCiLonghand (bracket id :: HS String String) "1" `shouldReturn` "1")
        describe "category" $ do
            it "composes" $
                executeGHCiLonghand (id :: HS String String) "1" `shouldReturn` "1"
        describe "cartesian" $ do
            it "copies" $
                executeGHCiLonghand (copy :: HS String (String, String)) "1" `shouldReturn` ("1", "1")
            it "consumes" $
                executeGHCiLonghand (consume :: HS String ()) "1" `shouldReturn` ()
            it "returns fst" $
                executeGHCiLonghand (fst' :: HS (String, Int) String) ("1", 1) `shouldReturn` "1"
            it "returns snd" $
                executeGHCiLonghand (snd' :: HS (Int, String) String) (1, "1") `shouldReturn` "1"
        describe "cocartesian" $ do
            it "injects Left" $ do
                executeGHCiLonghand (injectL :: HS String (Either String ())) "1" `shouldReturn` Left "1"
            it "injects Right" $ do
                executeGHCiLonghand (injectR :: HS String (Either () String)) "1" `shouldReturn` Right "1"
            describe "unify" $ do
                it "unifies Left" $
                    executeGHCiLonghand (unify :: HS (Either String String) String) (Left "1") `shouldReturn` "1"
                it "unifies Right" $
                    executeGHCiLonghand (unify :: HS (Either String String) String) (Right "1") `shouldReturn` "1"
            describe "tag" $ do
                it "tags Left" $
                    executeGHCiLonghand (tag :: HS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
                it "tags Right" $
                    executeGHCiLonghand (tag :: HS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
        describe "strong" $ do
            it "runs on first" $
                executeGHCiLonghand (first' copy :: HS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
            it "runs on second" $
                executeGHCiLonghand (second' copy :: HS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
        describe "choice" $ do
            describe "left'" $ pure ()
                -- it "runs on left" $
                --     executeGHCiLonghand (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn`  (Left ("1", "1")))it "doesn't run on right" $ (executeGHCi (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` (Right 1))
            describe "right'" $ do
                it "doesn't run on left" $
                    executeGHCiLonghand (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
                it "runs on right" $
                    executeGHCiLonghand (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
        describe "symmetric" $ do
            it "swaps" $
                executeGHCiLonghand (swap :: HS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
            describe "swapEither" $ do
                it "swaps left" $
                    executeGHCiLonghand (swapEither :: HS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
                it "swaps right" $
                    executeGHCiLonghand (swapEither :: HS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
            it "reassocs" $
                executeGHCiLonghand (reassoc :: HS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
            describe "reassocEither" $ do
                it "reassocs Left" $
                    executeGHCiLonghand (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                it "reassocs Right (Left)" $
                    executeGHCiLonghand (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                it "reassoc Right (Right)" $
                    executeGHCiLonghand (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
        describe "primitive" $ do
            describe "eq" $ do
                it "equal" $
                    executeGHCiLonghand (eq :: HS (String, String) Bool) ("a", "a") `shouldReturn` True
                it "not equal" $
                    executeGHCiLonghand (eq :: HS (String, String) Bool) ("a", "b") `shouldReturn` False
            it "reverses string" $
                executeGHCiLonghand (reverseString :: HS String String) "abc" `shouldReturn` "cba"
        describe "primitiveconsole" $ pure ()
        describe "primitive extra" $ do
            it "converts int to string" $
                executeGHCiLonghand (intToString :: HS Int String) 1 `shouldReturn` "1"
            it "concats string" $
                executeGHCiLonghand (concatString :: HS (String, String) String) ("a", "b") `shouldReturn` "ab"
            it "returns const string" $ do
                executeGHCiLonghand (constString "a" :: HS () String) () `shouldReturn` "a"
        describe "primitivefile" $ do
            xit "reads /etc/passwd" $ do
                executeGHCiLonghand (readFile' :: HS String String) "/etc/passwd" >>= (`shouldSatisfy` ((> 5) . length))
            it "doesn't read /etc/shadow" $ do
                executeGHCiLonghand (readFile' :: HS String String) "/etc/shadow" `shouldThrow` anyIOException
            xit "writes a file and then reads it back" $ do
                executeGHCiLonghand (writeFile' :: HS (String, String) ()) ("bob", "hello")
                executeGHCiLonghand (readFile' :: HS String String) "bob" `shouldReturn` "hello"
            it "doesn't write /etc/shadow" $ do
                executeGHCiLonghand (writeFile' :: HS (String, String) ()) ("/etc/shadow", "") `shouldThrow` anyIOException
        describe "numeric" $ do
            it "returns const int" $ do
                executeGHCiLonghand (num 1 :: HS () Int) () `shouldReturn` 1
            it "negates" $ do
                executeGHCiLonghand (negate' :: HS Int Int) 1 `shouldReturn` (-1)
            it "adds" $ do
                executeGHCiLonghand (add :: HS (Int, Int) Int) (1, 2) `shouldReturn` 3
            it "mults" $ do
                executeGHCiLonghand (mult :: HS (Int, Int) Int) (2, 3) `shouldReturn` 6
            it "divs" $ do
                executeGHCiLonghand (div' :: HS (Int, Int) Int) (4, 2) `shouldReturn` 2
            it "mods" $ do
                executeGHCiLonghand (mod' :: HS (Int, Int) Int) (5, 2) `shouldReturn` 1
        it "returns a string" $
            executeGHCiLonghand (id :: HS String String) "1" `shouldReturn` "1"
        it "returns an int" $
            executeGHCiLonghand (id :: HS Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeGHCiLonghand (id :: HS Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeGHCiLonghand (id :: HS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeGHCiLonghand (id :: HS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeGHCiLonghand (id :: HS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeGHCiLonghand (id :: HS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeGHCiLonghand (id :: HS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
    describe "with imports" $ do
        -- describe "bracket" . it "is idempotent" $ (do
        --         executeGHCiImports (bracket id :: HS String String) "1" `shouldReturn` "1")
        describe "category" $ do
            it "composes" $
                executeGHCiImports (id :: HS String String) "1" `shouldReturn` "1"
        describe "cartesian" $ do
            it "copies" $
                executeGHCiImports (copy :: HS String (String, String)) "1" `shouldReturn` ("1", "1")
            it "consumes" $
                executeGHCiImports (consume :: HS String ()) "1" `shouldReturn` ()
            it "returns fst" $
                executeGHCiImports (fst' :: HS (String, Int) String) ("1", 1) `shouldReturn` "1"
            it "returns snd" $
                executeGHCiImports (snd' :: HS (Int, String) String) (1, "1") `shouldReturn` "1"
        describe "cocartesian" $ do
            it "injects Left" $ do
                executeGHCiImports (injectL :: HS String (Either String ())) "1" `shouldReturn` Left "1"
            it "injects Right" $ do
                executeGHCiImports (injectR :: HS String (Either () String)) "1" `shouldReturn` Right "1"
            describe "unify" $ do
                it "unifies Left" $
                    executeGHCiImports (unify :: HS (Either String String) String) (Left "1") `shouldReturn` "1"
                it "unifies Right" $
                    executeGHCiImports (unify :: HS (Either String String) String) (Right "1") `shouldReturn` "1"
            describe "tag" $ do
                it "tags Left" $
                    executeGHCiImports (tag :: HS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
                it "tags Right" $
                    executeGHCiImports (tag :: HS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
        describe "strong" $ do
            it "runs on first" $
                executeGHCiImports (first' copy :: HS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
            it "runs on second" $
                executeGHCiImports (second' copy :: HS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
        describe "choice" $ do
            describe "left'" $ pure ()
                -- it "runs on left" $
                --     executeGHCiImports (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn`  (Left ("1", "1")))it "doesn't run on right" $ (executeGHCi (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` (Right 1))
            describe "right'" $ do
                it "doesn't run on left" $
                    executeGHCiImports (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
                it "runs on right" $
                    executeGHCiImports (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
        describe "symmetric" $ do
            it "swaps" $
                executeGHCiImports (swap :: HS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
            describe "swapEither" $ do
                it "swaps left" $
                    executeGHCiImports (swapEither :: HS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
                it "swaps right" $
                    executeGHCiImports (swapEither :: HS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
            it "reassocs" $
                executeGHCiImports (reassoc :: HS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
            describe "reassocEither" $ do
                it "reassocs Left" $
                    executeGHCiImports (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                it "reassocs Right (Left)" $
                    executeGHCiImports (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                it "reassoc Right (Right)" $
                    executeGHCiImports (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
        describe "primitive" $ do
            describe "eq" $ do
                it "equal" $
                    executeGHCiImports (eq :: HS (String, String) Bool) ("a", "a") `shouldReturn` True
                it "not equal" $
                    executeGHCiImports (eq :: HS (String, String) Bool) ("a", "b") `shouldReturn` False
            it "reverses string" $
                executeGHCiImports (reverseString :: HS String String) "abc" `shouldReturn` "cba"
        describe "primitiveconsole" $ pure ()
        describe "primitive extra" $ do
            it "converts int to string" $
                executeGHCiImports (intToString :: HS Int String) 1 `shouldReturn` "1"
            it "concats string" $
                executeGHCiImports (concatString :: HS (String, String) String) ("a", "b") `shouldReturn` "ab"
            it "returns const string" $ do
                executeGHCiImports (constString "a" :: HS () String) () `shouldReturn` "a"
        describe "primitivefile" $ do
            xit "reads /etc/passwd" $ do
                executeGHCiImports (readFile' :: HS String String) "/etc/passwd" >>= (`shouldSatisfy` ((> 5) . length))
            it "doesn't read /etc/shadow" $ do
                executeGHCiImports (readFile' :: HS String String) "/etc/shadow" `shouldThrow` anyIOException
            xit "writes a file and then reads it back" $ do
                executeGHCiImports (writeFile' :: HS (String, String) ()) ("bob", "hello")
                executeGHCiImports (readFile' :: HS String String) "bob" `shouldReturn` "hello"
            it "doesn't write /etc/shadow" $ do
                executeGHCiImports (writeFile' :: HS (String, String) ()) ("/etc/shadow", "") `shouldThrow` anyIOException
        describe "numeric" $ do
            it "returns const int" $ do
                executeGHCiImports (num 1 :: HS () Int) () `shouldReturn` 1
            it "negates" $ do
                executeGHCiImports (negate' :: HS Int Int) 1 `shouldReturn` (-1)
            it "adds" $ do
                executeGHCiImports (add :: HS (Int, Int) Int) (1, 2) `shouldReturn` 3
            it "mults" $ do
                executeGHCiImports (mult :: HS (Int, Int) Int) (2, 3) `shouldReturn` 6
            it "divs" $ do
                executeGHCiImports (div' :: HS (Int, Int) Int) (4, 2) `shouldReturn` 2
            it "mods" $ do
                executeGHCiImports (mod' :: HS (Int, Int) Int) (5, 2) `shouldReturn` 1
        it "returns a string" $
            executeGHCiImports (id :: HS String String) "1" `shouldReturn` "1"
        it "returns an int" $
            executeGHCiImports (id :: HS Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeGHCiImports (id :: HS Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeGHCiImports (id :: HS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeGHCiImports (id :: HS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeGHCiImports (id :: HS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeGHCiImports (id :: HS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeGHCiImports (id :: HS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1

