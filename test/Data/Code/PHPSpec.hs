module Data.Code.PHPSpec where
{-}
import Control.Category
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.JSON
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
import Prelude                            hiding (id, (.))
-}
import Test.Hspec hiding (runIO)
{-}
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
-}

spec ∷ Spec
spec = pure ()

{-xdescribe "PHP" $ do
    describe "bracket" . it "is idempotent" $ (executeJSON (bracket id :: PHP String String) "1" `shouldReturn` "1")
    describe "category" $ do
        it "composes" $
            executeJSON (id :: PHP String String) "1" `shouldReturn` "1"
    describe "cartesian" $ do
        it "copies" $
            executeJSON (copy :: PHP String (String, String)) "1" `shouldReturn` ("1", "1")
        it "consumes" $
            executeJSON (consume :: PHP String ()) "1" `shouldReturn` ()
        it "returns fst" $
            executeJSON (fst' :: PHP (String, Int) String) ("1", 1) `shouldReturn` "1"
        it "returns snd" $
            executeJSON (snd' :: PHP (Int, String) String) (1, "1") `shouldReturn` "1"
    describe "cocartesian" $ do
        it "injects Left" $ do
            executeJSON (injectL :: PHP String (Either String ())) "1" `shouldReturn` Left "1"
        it "injects Right" $ do
            executeJSON (injectR :: PHP String (Either () String)) "1" `shouldReturn` Right "1"
        describe "unify" $ do
            it "unifies Left" $
                executeJSON (unify :: PHP (Either String String) String) (Left "1") `shouldReturn` "1"
            it "unifies Right" $
                executeJSON (unify :: PHP (Either String String) String) (Right "1") `shouldReturn` "1"
        describe "tag" $ do
            it "tags Left" $
                executeJSON (tag :: PHP (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
            it "tags Right" $
                executeJSON (tag :: PHP (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
    describe "strong" $ do
        it "runs on first" $
            executeJSON (first' copy :: PHP (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
        it "runs on second" $
            executeJSON (second' copy :: PHP (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
    describe "choice" $ do
        xdescribe "left'" $ pure ()
            -- it "runs on left" $
            --     executeJSON (left' copy :: PHP (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn` (Right (Left ("1", "1")))it "doesn't run on right" $ (executeJSON (left' copy :: PHP (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` (Right 1))
        describe "right'" $ do
            it "doesn't run on left" $
                executeJSON (right' copy :: PHP (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
            it "runs on right" $
                executeJSON (right' copy :: PHP (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
    describe "symmetric" $ do
        it "swaps" $
            executeJSON (swap :: PHP (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
        describe "swapEither" $ do
            it "swaps left" $
                executeJSON (swapEither :: PHP (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
            it "swaps right" $
                executeJSON (swapEither :: PHP (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
        it "reassocs" $
            executeJSON (reassoc :: PHP (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
        xdescribe "reassocEither" $ do
            it "reassocs Left" $
                executeJSON (reassocEither :: PHP (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
            it "reassocs Right (Left)" $
                executeJSON (reassocEither :: PHP (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
            it "reassoc Right (Right)" $
                executeJSON (reassocEither :: PHP (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
    describe "primitive" $ do
        describe "eq" $ do
            it "equal" $
                executeJSON (eq :: PHP (String, String) Bool) ("a", "a") `shouldReturn` True
            it "not equal" $
                executeJSON (eq :: PHP (String, String) Bool) ("a", "b") `shouldReturn` False
        it "reverses string" $
            executeJSON (reverseString :: PHP String String) "abc" `shouldReturn` "cba"
    describe "primitiveconsole" $ pure ()
    describe "primitive extra" $ do
        it "converts int to string" $
            executeJSON (intToString :: PHP Int String) 1 `shouldReturn` "1"
        it "concats string" $
            executeJSON (concatString :: PHP (String, String) String) ("a", "b") `shouldReturn` "ab"
        it "returns const string" $ do
            executeJSON (constString "a" :: PHP () String) () `shouldReturn` "a"
    describe "numeric" $ do
        it "returns const int" $ do
            executeJSON (num 1 :: PHP () Int) () `shouldReturn` 1
        it "negates" $ do
            executeJSON (negate' :: PHP Int Int) 1 `shouldReturn` (-1)
        it "adds" $ do
            executeJSON (add :: PHP (Int, Int) Int) (1, 2) `shouldReturn` 3
        it "mults" $ do
            executeJSON (mult :: PHP (Int, Int) Int) (2, 3) `shouldReturn` 6
        it "divs" $ do
            executeJSON (div' :: PHP (Int, Int) Int) (4, 2) `shouldReturn` 2
        it "mods" $ do
            executeJSON (mod' :: PHP (Int, Int) Int) (5, 2) `shouldReturn` 1
    describe "executeJSON" $ do
        it "returns a string" $
            executeJSON (id :: PHP String String) "1" `shouldReturn` "1"
        it "returns an int" $
            executeJSON (id :: PHP Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeJSON (id :: PHP Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeJSON (id :: PHP (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeJSON (id :: PHP (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeJSON (id :: PHP (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeJSON (id :: PHP (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeJSON (id :: PHP (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1

-}
