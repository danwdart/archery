module Data.Code.HaskellSpec where

import Control.Category
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.Haskell
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
    describe "bracket" . it "is idempotent" $ (executeViaGHCi (bracket id :: HS String String) "1" `shouldReturn` "1")
    describe "category" $ do
        it "composes" $
            executeViaGHCi (id :: HS String String) "1" `shouldReturn` "1"
    describe "cartesian" $ do
        it "copies" $
            executeViaGHCi (copy :: HS String (String, String)) "1" `shouldReturn` ("1", "1")
        it "consumes" $
            executeViaGHCi (consume :: HS String ()) "1" `shouldReturn` ()
        it "returns fst" $
            executeViaGHCi (fst' :: HS (String, Int) String) ("1", 1) `shouldReturn` "1"
        it "returns snd" $
            executeViaGHCi (snd' :: HS (Int, String) String) (1, "1") `shouldReturn` "1"
    describe "cocartesian" $ do
        it "injects Left" $ do
            executeViaGHCi (injectL :: HS String (Either String ())) "1" `shouldReturn` Left "1"
        it "injects Right" $ do
            executeViaGHCi (injectR :: HS String (Either () String)) "1" `shouldReturn` Right "1"
        describe "unify" $ do
            it "unifies Left" $
                executeViaGHCi (unify :: HS (Either String String) String) (Left "1") `shouldReturn` "1"
            it "unifies Right" $
                executeViaGHCi (unify :: HS (Either String String) String) (Right "1") `shouldReturn` "1"
        describe "tag" $ do
            it "tags Left" $
                executeViaGHCi (tag :: HS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
            it "tags Right" $
                executeViaGHCi (tag :: HS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
    describe "strong" $ do
        it "runs on first" $
            executeViaGHCi (first' copy :: HS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
        it "runs on second" $
            executeViaGHCi (second' copy :: HS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
    describe "choice" $ do
        describe "left'" $ pure ()
            -- it "runs on left" $
            --     executeViaGHCi (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn`  (Left ("1", "1")))it "doesn't run on right" $ (executeViaGHCi (left' copy :: HS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` (Right 1))
        describe "right'" $ do
            it "doesn't run on left" $
                executeViaGHCi (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
            it "runs on right" $
                executeViaGHCi (right' copy :: HS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
    describe "symmetric" $ do
        it "swaps" $
            executeViaGHCi (swap :: HS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
        describe "swapEither" $ do
            it "swaps left" $
                executeViaGHCi (swapEither :: HS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
            it "swaps right" $
                executeViaGHCi (swapEither :: HS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
        it "reassocs" $
            executeViaGHCi (reassoc :: HS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
        describe "reassocEither" $ do
            it "reassocs Left" $
                executeViaGHCi (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
            it "reassocs Right (Left)" $
                executeViaGHCi (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
            it "reassoc Right (Right)" $
                executeViaGHCi (reassocEither :: HS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
    describe "primitive" $ do
        describe "eq" $ do
            it "equal" $
                executeViaGHCi (eq :: HS (String, String) Bool) ("a", "a") `shouldReturn` True
            it "not equal" $
                executeViaGHCi (eq :: HS (String, String) Bool) ("a", "b") `shouldReturn` False
        it "reverses string" $
            executeViaGHCi (reverseString :: HS String String) "abc" `shouldReturn` "cba"
    describe "primitiveconsole" $ pure ()
    describe "primitive extra" $ do
        it "converts int to string" $
            executeViaGHCi (intToString :: HS Int String) 1 `shouldReturn` "1"
        it "concats string" $
            executeViaGHCi (concatString :: HS (String, String) String) ("a", "b") `shouldReturn` "ab"
        it "returns const string" $ do
            executeViaGHCi (constString "a" :: HS () String) () `shouldReturn` "a"
    describe "primitivefile" $ do
        xit "reads /etc/passwd" $ do
            executeViaGHCi (readFile' :: HS String String) "/etc/passwd" >>= (`shouldSatisfy` ((> 5) . length))
        it "doesn't read /etc/shadow" $ do
            executeViaGHCi (readFile' :: HS String String) "/etc/shadow" `shouldThrow` anyIOException
        xit "writes a file and then reads it back" $ do
            executeViaGHCi (writeFile' :: HS (String, String) ()) ("bob", "hello")
            executeViaGHCi (readFile' :: HS String String) "bob" `shouldReturn` "hello"
        it "doesn't write /etc/shadow" $ do
            executeViaGHCi (writeFile' :: HS (String, String) ()) ("/etc/shadow", "") `shouldThrow` anyIOException
    describe "numeric" $ do
        it "returns const int" $ do
            executeViaGHCi (num 1 :: HS () Int) () `shouldReturn` 1
        it "negates" $ do
            executeViaGHCi (negate' :: HS Int Int) 1 `shouldReturn` (-1)
        it "adds" $ do
            executeViaGHCi (add :: HS (Int, Int) Int) (1, 2) `shouldReturn` 3
        it "mults" $ do
            executeViaGHCi (mult :: HS (Int, Int) Int) (2, 3) `shouldReturn` 6
        it "divs" $ do
            executeViaGHCi (div' :: HS (Int, Int) Int) (4, 2) `shouldReturn` 2
        it "mods" $ do
            executeViaGHCi (mod' :: HS (Int, Int) Int) (5, 2) `shouldReturn` 1
    describe "executeViaGHCi" $ do
        it "returns a string" $
            executeViaGHCi (id :: HS String String) "1" `shouldReturn` "1"
        it "returns an int" $
            executeViaGHCi (id :: HS Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeViaGHCi (id :: HS Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeViaGHCi (id :: HS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeViaGHCi (id :: HS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeViaGHCi (id :: HS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeViaGHCi (id :: HS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeViaGHCi (id :: HS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1

