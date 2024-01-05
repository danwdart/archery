module Data.Code.HaskellSpec where

import Control.Category
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.Haskell.WithDefinitions
import Control.Category.Execute.Haskell.WithImports
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
import Prelude                            hiding (id, (.))
import Test.Hspec                         hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec ∷ Spec
spec = describe "HS" $ do
    describe "with definitions" $ do
        describe "bracket" $
            it "is idempotent" $ do
                executeViaGHCiWithDefinitions (bracket id :: HS String String) "1" `shouldReturn` "1"
        describe "category" $ do
            it "composes" $
                executeViaGHCiWithDefinitions (id :: HS String String) "1" `shouldReturn` "1"
        describe "cartesian" $ do
            it "copies" $
                executeViaGHCiWithDefinitions (copy :: HS String (String, String)) "1" `shouldReturn` ("1", "1")
            it "consumes" $
                executeViaGHCiWithDefinitions (consume :: HS String ()) "1" `shouldReturn` ()
            it "returns fst" $
                executeViaGHCiWithDefinitions (fst' :: HS (String, Int) String) ("1", 1) `shouldReturn` "1"
            it "returns snd" $
                executeViaGHCiWithDefinitions (snd' :: HS (Int, String) String) (1, "1") `shouldReturn` "1"
        describe "cocartesian" $ do
            it "injects Left" $ do
                executeViaGHCiWithDefinitions (injectL :: HS String (Either String ())) "1" `shouldReturn` Left "1"
            it "injects Right" $ do
                executeViaGHCiWithDefinitions (injectR :: HS String (Either () String)) "1" `shouldReturn` Right "1"
            describe "unify" $ do
                it "unifies Left" $
                    executeViaGHCiWithDefinitions (unify :: HS (Either String String) String) (Left "1") `shouldReturn` "1"
                it "unifies Right" $
                    executeViaGHCiWithDefinitions (unify :: HS (Either String String) String) (Right "1") `shouldReturn` "1"
            describe "tag" $ do
                it "tags Left" $
                    executeViaGHCiWithDefinitions (tag :: HS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
                it "tags Right" $
                    executeViaGHCiWithDefinitions (tag :: HS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
        describe "strong" $ do
            it "runs on first" $
                executeViaGHCiWithDefinitions (first' copy :: HS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
            it "runs on second" $
                executeViaGHCiWithDefinitions (second' copy :: HS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
        describe "choice" $ do
            describe "left'" $ pure ()
                -- it "runs on left" $
                --     executeViaGHCiWithDefinitions (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn`  (Left ("1", "1")))it "doesn't run on right" $ (executeViaGHCi (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` (Right 1))
            describe "right'" $ do
                it "doesn't run on left" $
                    executeViaGHCiWithDefinitions (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
                it "runs on right" $
                    executeViaGHCiWithDefinitions (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
        describe "symmetric" $ do
            it "swaps" $
                executeViaGHCiWithDefinitions (swap :: HS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
            describe "swapEither" $ do
                it "swaps left" $
                    executeViaGHCiWithDefinitions (swapEither :: HS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
                it "swaps right" $
                    executeViaGHCiWithDefinitions (swapEither :: HS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
            it "reassocs" $
                executeViaGHCiWithDefinitions (reassoc :: HS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
            describe "reassocEither" $ do
                it "reassocs Left" $
                    executeViaGHCiWithDefinitions (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                it "reassocs Right (Left)" $
                    executeViaGHCiWithDefinitions (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                it "reassoc Right (Right)" $
                    executeViaGHCiWithDefinitions (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
        describe "primitive" $ do
            describe "eq" $ do
                it "equal" $
                    executeViaGHCiWithDefinitions (eq :: HS (String, String) Bool) ("a", "a") `shouldReturn` True
                it "not equal" $
                    executeViaGHCiWithDefinitions (eq :: HS (String, String) Bool) ("a", "b") `shouldReturn` False
            it "reverses string" $
                executeViaGHCiWithDefinitions (reverseString :: HS String String) "abc" `shouldReturn` "cba"
        describe "primitiveconsole" $ pure ()
        describe "primitive extra" $ do
            it "converts int to string" $
                executeViaGHCiWithDefinitions (intToString :: HS Int String) 1 `shouldReturn` "1"
            it "concats string" $
                executeViaGHCiWithDefinitions (concatString :: HS (String, String) String) ("a", "b") `shouldReturn` "ab"
            it "returns const string" $ do
                executeViaGHCiWithDefinitions (constString "a" :: HS () String) () `shouldReturn` "a"
        describe "primitivefile" $ do
            xit "reads /etc/passwd" $ do
                executeViaGHCiWithDefinitions (readFile' :: HS String String) "/etc/passwd" >>= (`shouldSatisfy` ((> 5) . length))
            it "doesn't read /etc/shadow" $ do
                executeViaGHCiWithDefinitions (readFile' :: HS String String) "/etc/shadow" `shouldThrow` anyIOException
            xit "writes a file and then reads it back" $ do
                executeViaGHCiWithDefinitions (writeFile' :: HS (String, String) ()) ("bob", "hello")
                executeViaGHCiWithDefinitions (readFile' :: HS String String) "bob" `shouldReturn` "hello"
            it "doesn't write /etc/shadow" $ do
                executeViaGHCiWithDefinitions (writeFile' :: HS (String, String) ()) ("/etc/shadow", "") `shouldThrow` anyIOException
        describe "numeric" $ do
            it "returns const int" $ do
                executeViaGHCiWithDefinitions (num 1 :: HS () Int) () `shouldReturn` 1
            it "negates" $ do
                executeViaGHCiWithDefinitions (negate' :: HS Int Int) 1 `shouldReturn` (-1)
            it "adds" $ do
                executeViaGHCiWithDefinitions (add :: HS (Int, Int) Int) (1, 2) `shouldReturn` 3
            it "mults" $ do
                executeViaGHCiWithDefinitions (mult :: HS (Int, Int) Int) (2, 3) `shouldReturn` 6
            it "divs" $ do
                executeViaGHCiWithDefinitions (div' :: HS (Int, Int) Int) (4, 2) `shouldReturn` 2
            it "mods" $ do
                executeViaGHCiWithDefinitions (mod' :: HS (Int, Int) Int) (5, 2) `shouldReturn` 1
        it "returns a string" $
            executeViaGHCiWithDefinitions (id :: HS String String) "1" `shouldReturn` "1"
        it "returns an int" $
            executeViaGHCiWithDefinitions (id :: HS Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeViaGHCiWithDefinitions (id :: HS Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeViaGHCiWithDefinitions (id :: HS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeViaGHCiWithDefinitions (id :: HS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeViaGHCiWithDefinitions (id :: HS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeViaGHCiWithDefinitions (id :: HS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeViaGHCiWithDefinitions (id :: HS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
    xdescribe "with imports" $ do
        describe "bracket" $
            it "is idempotent" $ do
                executeViaGHCiWithImports (bracket id :: HS String String) "1" `shouldReturn` "1"
        describe "category" $ do
            it "composes" $
                executeViaGHCiWithImports (id :: HS String String) "1" `shouldReturn` "1"
        describe "cartesian" $ do
            it "copies" $
                executeViaGHCiWithImports (copy :: HS String (String, String)) "1" `shouldReturn` ("1", "1")
            it "consumes" $
                executeViaGHCiWithImports (consume :: HS String ()) "1" `shouldReturn` ()
            it "returns fst" $
                executeViaGHCiWithImports (fst' :: HS (String, Int) String) ("1", 1) `shouldReturn` "1"
            it "returns snd" $
                executeViaGHCiWithImports (snd' :: HS (Int, String) String) (1, "1") `shouldReturn` "1"
        describe "cocartesian" $ do
            it "injects Left" $ do
                executeViaGHCiWithImports (injectL :: HS String (Either String ())) "1" `shouldReturn` Left "1"
            it "injects Right" $ do
                executeViaGHCiWithImports (injectR :: HS String (Either () String)) "1" `shouldReturn` Right "1"
            describe "unify" $ do
                it "unifies Left" $
                    executeViaGHCiWithImports (unify :: HS (Either String String) String) (Left "1") `shouldReturn` "1"
                it "unifies Right" $
                    executeViaGHCiWithImports (unify :: HS (Either String String) String) (Right "1") `shouldReturn` "1"
            describe "tag" $ do
                it "tags Left" $
                    executeViaGHCiWithImports (tag :: HS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
                it "tags Right" $
                    executeViaGHCiWithImports (tag :: HS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
        describe "strong" $ do
            it "runs on first" $
                executeViaGHCiWithImports (first' copy :: HS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
            it "runs on second" $
                executeViaGHCiWithImports (second' copy :: HS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
        describe "choice" $ do
            describe "left'" $ pure ()
                -- it "runs on left" $
                --     executeViaGHCiWithImports (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn`  (Left ("1", "1")))it "doesn't run on right" $ (executeViaGHCi (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` (Right 1))
            describe "right'" $ do
                it "doesn't run on left" $
                    executeViaGHCiWithImports (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
                it "runs on right" $
                    executeViaGHCiWithImports (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
        describe "symmetric" $ do
            it "swaps" $
                executeViaGHCiWithImports (swap :: HS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
            describe "swapEither" $ do
                it "swaps left" $
                    executeViaGHCiWithImports (swapEither :: HS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
                it "swaps right" $
                    executeViaGHCiWithImports (swapEither :: HS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
            it "reassocs" $
                executeViaGHCiWithImports (reassoc :: HS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
            describe "reassocEither" $ do
                it "reassocs Left" $
                    executeViaGHCiWithImports (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                it "reassocs Right (Left)" $
                    executeViaGHCiWithImports (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                it "reassoc Right (Right)" $
                    executeViaGHCiWithImports (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
        describe "primitive" $ do
            describe "eq" $ do
                it "equal" $
                    executeViaGHCiWithImports (eq :: HS (String, String) Bool) ("a", "a") `shouldReturn` True
                it "not equal" $
                    executeViaGHCiWithImports (eq :: HS (String, String) Bool) ("a", "b") `shouldReturn` False
            it "reverses string" $
                executeViaGHCiWithImports (reverseString :: HS String String) "abc" `shouldReturn` "cba"
        describe "primitiveconsole" $ pure ()
        describe "primitive extra" $ do
            it "converts int to string" $
                executeViaGHCiWithImports (intToString :: HS Int String) 1 `shouldReturn` "1"
            it "concats string" $
                executeViaGHCiWithImports (concatString :: HS (String, String) String) ("a", "b") `shouldReturn` "ab"
            it "returns const string" $ do
                executeViaGHCiWithImports (constString "a" :: HS () String) () `shouldReturn` "a"
        describe "primitivefile" $ do
            xit "reads /etc/passwd" $ do
                executeViaGHCiWithImports (readFile' :: HS String String) "/etc/passwd" >>= (`shouldSatisfy` ((> 5) . length))
            it "doesn't read /etc/shadow" $ do
                executeViaGHCiWithImports (readFile' :: HS String String) "/etc/shadow" `shouldThrow` anyIOException
            xit "writes a file and then reads it back" $ do
                executeViaGHCiWithImports (writeFile' :: HS (String, String) ()) ("bob", "hello")
                executeViaGHCiWithImports (readFile' :: HS String String) "bob" `shouldReturn` "hello"
            it "doesn't write /etc/shadow" $ do
                executeViaGHCiWithImports (writeFile' :: HS (String, String) ()) ("/etc/shadow", "") `shouldThrow` anyIOException
        describe "numeric" $ do
            it "returns const int" $ do
                executeViaGHCiWithImports (num 1 :: HS () Int) () `shouldReturn` 1
            it "negates" $ do
                executeViaGHCiWithImports (negate' :: HS Int Int) 1 `shouldReturn` (-1)
            it "adds" $ do
                executeViaGHCiWithImports (add :: HS (Int, Int) Int) (1, 2) `shouldReturn` 3
            it "mults" $ do
                executeViaGHCiWithImports (mult :: HS (Int, Int) Int) (2, 3) `shouldReturn` 6
            it "divs" $ do
                executeViaGHCiWithImports (div' :: HS (Int, Int) Int) (4, 2) `shouldReturn` 2
            it "mods" $ do
                executeViaGHCiWithImports (mod' :: HS (Int, Int) Int) (5, 2) `shouldReturn` 1
        it "returns a string" $
            executeViaGHCiWithImports (id :: HS String String) "1" `shouldReturn` "1"
        it "returns an int" $
            executeViaGHCiWithImports (id :: HS Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeViaGHCiWithImports (id :: HS Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeViaGHCiWithImports (id :: HS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeViaGHCiWithImports (id :: HS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeViaGHCiWithImports (id :: HS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeViaGHCiWithImports (id :: HS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeViaGHCiWithImports (id :: HS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1

