module Data.Code.HaskellSpec (spec) where

import Control.Category
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.Haskell.Imports
import Control.Category.Execute.Haskell.Longhand
import Control.Category.Execute.Haskell.Shorthand
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
import Data.Code.Generic
import Data.Code.Haskell
import Prelude                                    hiding (id, (.))
import Test.Hspec                                 hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic                    ()

spec ∷ Spec
spec = describe "HS" $ do
    describe "GHCi" $ do
        describe "with Longhand" $ do
            -- describe "bracket" . it "is idempotent" $ (do
            --         executeGHCiLonghand (bracket id :: HS String String) "1" `shouldReturn` "1")
            describe "category" $ do
                it "ids" $
                    executeGHCiLonghand (id :: HS String String) "1" `shouldReturn` "1"
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
                describe "left'" $ do
                    it "runs on left" $
                        executeGHCiLonghand (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn`  Left ("1", "1")
                    it "doesn't run on right" $
                        executeGHCiLonghand (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` Right 1
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
        describe "with Shorthand" $ do
            -- describe "bracket" . it "is idempotent" $ (do
            --         executeGHCiShorthand (bracket id :: HS String String) "1" `shouldReturn` "1")
            describe "category" $ do
                it "ids" $
                    executeGHCiShorthand (id :: HS String String) "1" `shouldReturn` "1"
                it "composes" $
                    executeGHCiShorthand (id :: HS String String) "1" `shouldReturn` "1"
            describe "cartesian" $ do
                it "copies" $
                    executeGHCiShorthand (copy :: HS String (String, String)) "1" `shouldReturn` ("1", "1")
                it "consumes" $
                    executeGHCiShorthand (consume :: HS String ()) "1" `shouldReturn` ()
                it "returns fst" $
                    executeGHCiShorthand (fst' :: HS (String, Int) String) ("1", 1) `shouldReturn` "1"
                it "returns snd" $
                    executeGHCiShorthand (snd' :: HS (Int, String) String) (1, "1") `shouldReturn` "1"
            describe "cocartesian" $ do
                it "injects Left" $ do
                    executeGHCiShorthand (injectL :: HS String (Either String ())) "1" `shouldReturn` Left "1"
                it "injects Right" $ do
                    executeGHCiShorthand (injectR :: HS String (Either () String)) "1" `shouldReturn` Right "1"
                describe "unify" $ do
                    it "unifies Left" $
                        executeGHCiShorthand (unify :: HS (Either String String) String) (Left "1") `shouldReturn` "1"
                    it "unifies Right" $
                        executeGHCiShorthand (unify :: HS (Either String String) String) (Right "1") `shouldReturn` "1"
                describe "tag" $ do
                    it "tags Left" $
                        executeGHCiShorthand (tag :: HS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
                    it "tags Right" $
                        executeGHCiShorthand (tag :: HS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
            describe "strong" $ do
                it "runs on first" $
                    executeGHCiShorthand (first' copy :: HS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
                it "runs on second" $
                    executeGHCiShorthand (second' copy :: HS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
            describe "choice" $ do
                describe "left'" $ do
                    it "runs on left" $
                        executeGHCiShorthand (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn`  Left ("1", "1")
                    it "doesn't run on right" $
                        executeGHCiShorthand (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` Right 1
                describe "right'" $ do
                    it "doesn't run on left" $
                        executeGHCiShorthand (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
                    it "runs on right" $
                        executeGHCiShorthand (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
            describe "symmetric" $ do
                it "swaps" $
                    executeGHCiShorthand (swap :: HS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
                describe "swapEither" $ do
                    it "swaps left" $
                        executeGHCiShorthand (swapEither :: HS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
                    it "swaps right" $
                        executeGHCiShorthand (swapEither :: HS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
                it "reassocs" $
                    executeGHCiShorthand (reassoc :: HS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
                describe "reassocEither" $ do
                    it "reassocs Left" $
                        executeGHCiShorthand (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                    it "reassocs Right (Left)" $
                        executeGHCiShorthand (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                    it "reassoc Right (Right)" $
                        executeGHCiShorthand (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
            describe "primitive" $ do
                describe "eq" $ do
                    it "equal" $
                        executeGHCiShorthand (eq :: HS (String, String) Bool) ("a", "a") `shouldReturn` True
                    it "not equal" $
                        executeGHCiShorthand (eq :: HS (String, String) Bool) ("a", "b") `shouldReturn` False
                it "reverses string" $
                    executeGHCiShorthand (reverseString :: HS String String) "abc" `shouldReturn` "cba"
            describe "primitiveconsole" $ pure ()
            describe "primitive extra" $ do
                it "converts int to string" $
                    executeGHCiShorthand (intToString :: HS Int String) 1 `shouldReturn` "1"
                it "concats string" $
                    executeGHCiShorthand (concatString :: HS (String, String) String) ("a", "b") `shouldReturn` "ab"
                it "returns const string" $ do
                    executeGHCiShorthand (constString "a" :: HS () String) () `shouldReturn` "a"
            describe "primitivefile" $ do
                xit "reads /etc/passwd" $ do
                    executeGHCiShorthand (readFile' :: HS String String) "/etc/passwd" >>= (`shouldSatisfy` ((> 5) . length))
                it "doesn't read /etc/shadow" $ do
                    executeGHCiShorthand (readFile' :: HS String String) "/etc/shadow" `shouldThrow` anyIOException
                xit "writes a file and then reads it back" $ do
                    executeGHCiShorthand (writeFile' :: HS (String, String) ()) ("bob", "hello")
                    executeGHCiShorthand (readFile' :: HS String String) "bob" `shouldReturn` "hello"
                it "doesn't write /etc/shadow" $ do
                    executeGHCiShorthand (writeFile' :: HS (String, String) ()) ("/etc/shadow", "") `shouldThrow` anyIOException
            describe "numeric" $ do
                it "returns const int" $ do
                    executeGHCiShorthand (num 1 :: HS () Int) () `shouldReturn` 1
                it "negates" $ do
                    executeGHCiShorthand (negate' :: HS Int Int) 1 `shouldReturn` (-1)
                it "adds" $ do
                    executeGHCiShorthand (add :: HS (Int, Int) Int) (1, 2) `shouldReturn` 3
                it "mults" $ do
                    executeGHCiShorthand (mult :: HS (Int, Int) Int) (2, 3) `shouldReturn` 6
                it "divs" $ do
                    executeGHCiShorthand (div' :: HS (Int, Int) Int) (4, 2) `shouldReturn` 2
                it "mods" $ do
                    executeGHCiShorthand (mod' :: HS (Int, Int) Int) (5, 2) `shouldReturn` 1
            it "returns a string" $
                executeGHCiShorthand (id :: HS String String) "1" `shouldReturn` "1"
            it "returns an int" $
                executeGHCiShorthand (id :: HS Int Int) 1 `shouldReturn` 1
            it "returns a bool" $
                executeGHCiShorthand (id :: HS Bool Bool) True `shouldReturn` True
            it "returns a tuple" $
                executeGHCiShorthand (id :: HS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
            describe "Either" $ do
                it "returns a Left" $
                    executeGHCiShorthand (id :: HS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
                it "returns a Right" $
                    executeGHCiShorthand (id :: HS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
            describe "Maybe" $ do
                it "returns a Nothing" $
                    executeGHCiShorthand (id :: HS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
                it "returns a Just" $
                    executeGHCiShorthand (id :: HS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
        xdescribe "with imports" $ do
            -- describe "bracket" . it "is idempotent" $ (do
            --         executeGHCiImports (bracket id :: HS String String) "1" `shouldReturn` "1")
            describe "category" $ do
                xit "ids" $
                    executeGHCiImports (id :: HS String String) "1" `shouldReturn` "1"
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
                describe "left'" $ do
                    it "runs on left" $
                        executeGHCiImports (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn`  Left ("1", "1")
                    it "doesn't run on right" $
                        executeGHCiImports (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` Right 1
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
    xdescribe "JSON" $ do
        describe "with Longhand" $ do
            -- describe "bracket" . it "is idempotent" $ (do
            --         executeJSONLonghand (bracket id :: HS String String) "1" `shouldReturn` "1")
            describe "category" $ do
                it "ids" $
                    executeJSONLonghand (id :: HS String String) "1" `shouldReturn` "1"
                it "composes" $
                    executeJSONLonghand (id :: HS String String) "1" `shouldReturn` "1"
            describe "cartesian" $ do
                it "copies" $
                    executeJSONLonghand (copy :: HS String (String, String)) "1" `shouldReturn` ("1", "1")
                it "consumes" $
                    executeJSONLonghand (consume :: HS String ()) "1" `shouldReturn` ()
                it "returns fst" $
                    executeJSONLonghand (fst' :: HS (String, Int) String) ("1", 1) `shouldReturn` "1"
                it "returns snd" $
                    executeJSONLonghand (snd' :: HS (Int, String) String) (1, "1") `shouldReturn` "1"
            describe "cocartesian" $ do
                it "injects Left" $ do
                    executeJSONLonghand (injectL :: HS String (Either String ())) "1" `shouldReturn` Left "1"
                it "injects Right" $ do
                    executeJSONLonghand (injectR :: HS String (Either () String)) "1" `shouldReturn` Right "1"
                describe "unify" $ do
                    it "unifies Left" $
                        executeJSONLonghand (unify :: HS (Either String String) String) (Left "1") `shouldReturn` "1"
                    it "unifies Right" $
                        executeJSONLonghand (unify :: HS (Either String String) String) (Right "1") `shouldReturn` "1"
                describe "tag" $ do
                    it "tags Left" $
                        executeJSONLonghand (tag :: HS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
                    it "tags Right" $
                        executeJSONLonghand (tag :: HS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
            describe "strong" $ do
                it "runs on first" $
                    executeJSONLonghand (first' copy :: HS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
                it "runs on second" $
                    executeJSONLonghand (second' copy :: HS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
            describe "choice" $ do
                describe "left'" $ do
                    it "runs on left" $
                        executeJSONLonghand (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn`  Left ("1", "1")
                    it "doesn't run on right" $
                        executeJSONLonghand (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` Right 1
                describe "right'" $ do
                    it "doesn't run on left" $
                        executeJSONLonghand (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
                    it "runs on right" $
                        executeJSONLonghand (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
            describe "symmetric" $ do
                it "swaps" $
                    executeJSONLonghand (swap :: HS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
                describe "swapEither" $ do
                    it "swaps left" $
                        executeJSONLonghand (swapEither :: HS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
                    it "swaps right" $
                        executeJSONLonghand (swapEither :: HS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
                it "reassocs" $
                    executeJSONLonghand (reassoc :: HS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
                describe "reassocEither" $ do
                    it "reassocs Left" $
                        executeJSONLonghand (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                    it "reassocs Right (Left)" $
                        executeJSONLonghand (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                    it "reassoc Right (Right)" $
                        executeJSONLonghand (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
            describe "primitive" $ do
                describe "eq" $ do
                    it "equal" $
                        executeJSONLonghand (eq :: HS (String, String) Bool) ("a", "a") `shouldReturn` True
                    it "not equal" $
                        executeJSONLonghand (eq :: HS (String, String) Bool) ("a", "b") `shouldReturn` False
                it "reverses string" $
                    executeJSONLonghand (reverseString :: HS String String) "abc" `shouldReturn` "cba"
            describe "primitiveconsole" $ pure ()
            describe "primitive extra" $ do
                it "converts int to string" $
                    executeJSONLonghand (intToString :: HS Int String) 1 `shouldReturn` "1"
                it "concats string" $
                    executeJSONLonghand (concatString :: HS (String, String) String) ("a", "b") `shouldReturn` "ab"
                it "returns const string" $ do
                    executeJSONLonghand (constString "a" :: HS () String) () `shouldReturn` "a"
            describe "primitivefile" $ do
                xit "reads /etc/passwd" $ do
                    executeJSONLonghand (readFile' :: HS String String) "/etc/passwd" >>= (`shouldSatisfy` ((> 5) . length))
                it "doesn't read /etc/shadow" $ do
                    executeJSONLonghand (readFile' :: HS String String) "/etc/shadow" `shouldThrow` anyIOException
                xit "writes a file and then reads it back" $ do
                    executeJSONLonghand (writeFile' :: HS (String, String) ()) ("bob", "hello")
                    executeJSONLonghand (readFile' :: HS String String) "bob" `shouldReturn` "hello"
                it "doesn't write /etc/shadow" $ do
                    executeJSONLonghand (writeFile' :: HS (String, String) ()) ("/etc/shadow", "") `shouldThrow` anyIOException
            describe "numeric" $ do
                it "returns const int" $ do
                    executeJSONLonghand (num 1 :: HS () Int) () `shouldReturn` 1
                it "negates" $ do
                    executeJSONLonghand (negate' :: HS Int Int) 1 `shouldReturn` (-1)
                it "adds" $ do
                    executeJSONLonghand (add :: HS (Int, Int) Int) (1, 2) `shouldReturn` 3
                it "mults" $ do
                    executeJSONLonghand (mult :: HS (Int, Int) Int) (2, 3) `shouldReturn` 6
                it "divs" $ do
                    executeJSONLonghand (div' :: HS (Int, Int) Int) (4, 2) `shouldReturn` 2
                it "mods" $ do
                    executeJSONLonghand (mod' :: HS (Int, Int) Int) (5, 2) `shouldReturn` 1
            it "returns a string" $
                executeJSONLonghand (id :: HS String String) "1" `shouldReturn` "1"
            it "returns an int" $
                executeJSONLonghand (id :: HS Int Int) 1 `shouldReturn` 1
            it "returns a bool" $
                executeJSONLonghand (id :: HS Bool Bool) True `shouldReturn` True
            it "returns a tuple" $
                executeJSONLonghand (id :: HS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
            describe "Either" $ do
                it "returns a Left" $
                    executeJSONLonghand (id :: HS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
                it "returns a Right" $
                    executeJSONLonghand (id :: HS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
            describe "Maybe" $ do
                it "returns a Nothing" $
                    executeJSONLonghand (id :: HS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
                it "returns a Just" $
                    executeJSONLonghand (id :: HS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
        describe "with Shorthand" $ do
            -- describe "bracket" . it "is idempotent" $ (do
            --         executeJSONShorthand (bracket id :: HS String String) "1" `shouldReturn` "1")
            describe "category" $ do
                it "ids" $
                    executeJSONShorthand (id :: HS String String) "1" `shouldReturn` "1"
                it "composes" $
                    executeJSONShorthand (id :: HS String String) "1" `shouldReturn` "1"
            describe "cartesian" $ do
                it "copies" $
                    executeJSONShorthand (copy :: HS String (String, String)) "1" `shouldReturn` ("1", "1")
                it "consumes" $
                    executeJSONShorthand (consume :: HS String ()) "1" `shouldReturn` ()
                it "returns fst" $
                    executeJSONShorthand (fst' :: HS (String, Int) String) ("1", 1) `shouldReturn` "1"
                it "returns snd" $
                    executeJSONShorthand (snd' :: HS (Int, String) String) (1, "1") `shouldReturn` "1"
            describe "cocartesian" $ do
                it "injects Left" $ do
                    executeJSONShorthand (injectL :: HS String (Either String ())) "1" `shouldReturn` Left "1"
                it "injects Right" $ do
                    executeJSONShorthand (injectR :: HS String (Either () String)) "1" `shouldReturn` Right "1"
                describe "unify" $ do
                    it "unifies Left" $
                        executeJSONShorthand (unify :: HS (Either String String) String) (Left "1") `shouldReturn` "1"
                    it "unifies Right" $
                        executeJSONShorthand (unify :: HS (Either String String) String) (Right "1") `shouldReturn` "1"
                describe "tag" $ do
                    it "tags Left" $
                        executeJSONShorthand (tag :: HS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
                    it "tags Right" $
                        executeJSONShorthand (tag :: HS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
            describe "strong" $ do
                it "runs on first" $
                    executeJSONShorthand (first' copy :: HS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
                it "runs on second" $
                    executeJSONShorthand (second' copy :: HS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
            describe "choice" $ do
                describe "left'" $ do
                    it "runs on left" $
                        executeJSONShorthand (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn`  Left ("1", "1")
                    it "doesn't run on right" $
                        executeJSONShorthand (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` Right 1
                describe "right'" $ do
                    it "doesn't run on left" $
                        executeJSONShorthand (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
                    it "runs on right" $
                        executeJSONShorthand (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
            describe "symmetric" $ do
                it "swaps" $
                    executeJSONShorthand (swap :: HS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
                describe "swapEither" $ do
                    it "swaps left" $
                        executeJSONShorthand (swapEither :: HS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
                    it "swaps right" $
                        executeJSONShorthand (swapEither :: HS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
                it "reassocs" $
                    executeJSONShorthand (reassoc :: HS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
                describe "reassocEither" $ do
                    it "reassocs Left" $
                        executeJSONShorthand (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                    it "reassocs Right (Left)" $
                        executeJSONShorthand (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                    it "reassoc Right (Right)" $
                        executeJSONShorthand (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
            describe "primitive" $ do
                describe "eq" $ do
                    it "equal" $
                        executeJSONShorthand (eq :: HS (String, String) Bool) ("a", "a") `shouldReturn` True
                    it "not equal" $
                        executeJSONShorthand (eq :: HS (String, String) Bool) ("a", "b") `shouldReturn` False
                it "reverses string" $
                    executeJSONShorthand (reverseString :: HS String String) "abc" `shouldReturn` "cba"
            describe "primitiveconsole" $ pure ()
            describe "primitive extra" $ do
                it "converts int to string" $
                    executeJSONShorthand (intToString :: HS Int String) 1 `shouldReturn` "1"
                it "concats string" $
                    executeJSONShorthand (concatString :: HS (String, String) String) ("a", "b") `shouldReturn` "ab"
                it "returns const string" $ do
                    executeJSONShorthand (constString "a" :: HS () String) () `shouldReturn` "a"
            describe "primitivefile" $ do
                xit "reads /etc/passwd" $ do
                    executeJSONShorthand (readFile' :: HS String String) "/etc/passwd" >>= (`shouldSatisfy` ((> 5) . length))
                it "doesn't read /etc/shadow" $ do
                    executeJSONShorthand (readFile' :: HS String String) "/etc/shadow" `shouldThrow` anyIOException
                xit "writes a file and then reads it back" $ do
                    executeJSONShorthand (writeFile' :: HS (String, String) ()) ("bob", "hello")
                    executeJSONShorthand (readFile' :: HS String String) "bob" `shouldReturn` "hello"
                it "doesn't write /etc/shadow" $ do
                    executeJSONShorthand (writeFile' :: HS (String, String) ()) ("/etc/shadow", "") `shouldThrow` anyIOException
            describe "numeric" $ do
                it "returns const int" $ do
                    executeJSONShorthand (num 1 :: HS () Int) () `shouldReturn` 1
                it "negates" $ do
                    executeJSONShorthand (negate' :: HS Int Int) 1 `shouldReturn` (-1)
                it "adds" $ do
                    executeJSONShorthand (add :: HS (Int, Int) Int) (1, 2) `shouldReturn` 3
                it "mults" $ do
                    executeJSONShorthand (mult :: HS (Int, Int) Int) (2, 3) `shouldReturn` 6
                it "divs" $ do
                    executeJSONShorthand (div' :: HS (Int, Int) Int) (4, 2) `shouldReturn` 2
                it "mods" $ do
                    executeJSONShorthand (mod' :: HS (Int, Int) Int) (5, 2) `shouldReturn` 1
            it "returns a string" $
                executeJSONShorthand (id :: HS String String) "1" `shouldReturn` "1"
            it "returns an int" $
                executeJSONShorthand (id :: HS Int Int) 1 `shouldReturn` 1
            it "returns a bool" $
                executeJSONShorthand (id :: HS Bool Bool) True `shouldReturn` True
            it "returns a tuple" $
                executeJSONShorthand (id :: HS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
            describe "Either" $ do
                it "returns a Left" $
                    executeJSONShorthand (id :: HS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
                it "returns a Right" $
                    executeJSONShorthand (id :: HS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
            describe "Maybe" $ do
                it "returns a Nothing" $
                    executeJSONShorthand (id :: HS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
                it "returns a Just" $
                    executeJSONShorthand (id :: HS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
        xdescribe "with imports" $ do
            -- describe "bracket" . it "is idempotent" $ (do
            --         executeJSONImports (bracket id :: HS String String) "1" `shouldReturn` "1")
            describe "category" $ do
                xit "ids" $
                    executeJSONImports (id :: HS String String) "1" `shouldReturn` "1"
                it "composes" $
                    executeJSONImports (id :: HS String String) "1" `shouldReturn` "1"
            describe "cartesian" $ do
                it "copies" $
                    executeJSONImports (copy :: HS String (String, String)) "1" `shouldReturn` ("1", "1")
                it "consumes" $
                    executeJSONImports (consume :: HS String ()) "1" `shouldReturn` ()
                it "returns fst" $
                    executeJSONImports (fst' :: HS (String, Int) String) ("1", 1) `shouldReturn` "1"
                it "returns snd" $
                    executeJSONImports (snd' :: HS (Int, String) String) (1, "1") `shouldReturn` "1"
            describe "cocartesian" $ do
                it "injects Left" $ do
                    executeJSONImports (injectL :: HS String (Either String ())) "1" `shouldReturn` Left "1"
                it "injects Right" $ do
                    executeJSONImports (injectR :: HS String (Either () String)) "1" `shouldReturn` Right "1"
                describe "unify" $ do
                    it "unifies Left" $
                        executeJSONImports (unify :: HS (Either String String) String) (Left "1") `shouldReturn` "1"
                    it "unifies Right" $
                        executeJSONImports (unify :: HS (Either String String) String) (Right "1") `shouldReturn` "1"
                describe "tag" $ do
                    it "tags Left" $
                        executeJSONImports (tag :: HS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
                    it "tags Right" $
                        executeJSONImports (tag :: HS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
            describe "strong" $ do
                it "runs on first" $
                    executeJSONImports (first' copy :: HS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
                it "runs on second" $
                    executeJSONImports (second' copy :: HS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
            describe "choice" $ do
                describe "left'" $ do
                    it "runs on left" $
                        executeJSONImports (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn`  Left ("1", "1")
                    it "doesn't run on right" $
                        executeJSONImports (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` Right 1
                describe "right'" $ do
                    it "doesn't run on left" $
                        executeJSONImports (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
                    it "runs on right" $
                        executeJSONImports (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
            describe "symmetric" $ do
                it "swaps" $
                    executeJSONImports (swap :: HS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
                describe "swapEither" $ do
                    it "swaps left" $
                        executeJSONImports (swapEither :: HS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
                    it "swaps right" $
                        executeJSONImports (swapEither :: HS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
                it "reassocs" $
                    executeJSONImports (reassoc :: HS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
                describe "reassocEither" $ do
                    it "reassocs Left" $
                        executeJSONImports (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                    it "reassocs Right (Left)" $
                        executeJSONImports (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                    it "reassoc Right (Right)" $
                        executeJSONImports (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
            describe "primitive" $ do
                describe "eq" $ do
                    it "equal" $
                        executeJSONImports (eq :: HS (String, String) Bool) ("a", "a") `shouldReturn` True
                    it "not equal" $
                        executeJSONImports (eq :: HS (String, String) Bool) ("a", "b") `shouldReturn` False
                it "reverses string" $
                    executeJSONImports (reverseString :: HS String String) "abc" `shouldReturn` "cba"
            describe "primitiveconsole" $ pure ()
            describe "primitive extra" $ do
                it "converts int to string" $
                    executeJSONImports (intToString :: HS Int String) 1 `shouldReturn` "1"
                it "concats string" $
                    executeJSONImports (concatString :: HS (String, String) String) ("a", "b") `shouldReturn` "ab"
                it "returns const string" $ do
                    executeJSONImports (constString "a" :: HS () String) () `shouldReturn` "a"
            describe "primitivefile" $ do
                xit "reads /etc/passwd" $ do
                    executeJSONImports (readFile' :: HS String String) "/etc/passwd" >>= (`shouldSatisfy` ((> 5) . length))
                it "doesn't read /etc/shadow" $ do
                    executeJSONImports (readFile' :: HS String String) "/etc/shadow" `shouldThrow` anyIOException
                xit "writes a file and then reads it back" $ do
                    executeJSONImports (writeFile' :: HS (String, String) ()) ("bob", "hello")
                    executeJSONImports (readFile' :: HS String String) "bob" `shouldReturn` "hello"
                it "doesn't write /etc/shadow" $ do
                    executeJSONImports (writeFile' :: HS (String, String) ()) ("/etc/shadow", "") `shouldThrow` anyIOException
            describe "numeric" $ do
                it "returns const int" $ do
                    executeJSONImports (num 1 :: HS () Int) () `shouldReturn` 1
                it "negates" $ do
                    executeJSONImports (negate' :: HS Int Int) 1 `shouldReturn` (-1)
                it "adds" $ do
                    executeJSONImports (add :: HS (Int, Int) Int) (1, 2) `shouldReturn` 3
                it "mults" $ do
                    executeJSONImports (mult :: HS (Int, Int) Int) (2, 3) `shouldReturn` 6
                it "divs" $ do
                    executeJSONImports (div' :: HS (Int, Int) Int) (4, 2) `shouldReturn` 2
                it "mods" $ do
                    executeJSONImports (mod' :: HS (Int, Int) Int) (5, 2) `shouldReturn` 1
            it "returns a string" $
                executeJSONImports (id :: HS String String) "1" `shouldReturn` "1"
            it "returns an int" $
                executeJSONImports (id :: HS Int Int) 1 `shouldReturn` 1
            it "returns a bool" $
                executeJSONImports (id :: HS Bool Bool) True `shouldReturn` True
            it "returns a tuple" $
                executeJSONImports (id :: HS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
            describe "Either" $ do
                it "returns a Left" $
                    executeJSONImports (id :: HS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
                it "returns a Right" $
                    executeJSONImports (id :: HS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
            describe "Maybe" $ do
                it "returns a Nothing" $
                    executeJSONImports (id :: HS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
                it "returns a Just" $
                    executeJSONImports (id :: HS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
