module Data.Code.JSSpec where
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
import Data.Code.JS
import Prelude                            hiding (id, (.))
import Test.Hspec
{-}
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
-}
spec ∷ Spec
spec = describe "JS" $ do
    describe "executeJSONLonghand" $ do
        it "returns a string" $
            executeJSONLonghand (id :: JS String String) "1" `shouldReturn` "1"
        it "returns an int" $
            executeJSONLonghand (id :: JS Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeJSONLonghand (id :: JS Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeJSONLonghand (id :: JS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeJSONLonghand (id :: JS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeJSONLonghand (id :: JS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeJSONLonghand (id :: JS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeJSONLonghand (id :: JS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
        -- describe "bracket" .
        --     it "is idempotent" $
        --         executeJSONLonghand (bracket id :: JS String String) "1" `shouldReturn` "1"
        describe "category" $ do
            it "composes" $
                executeJSONLonghand (id :: JS String String) "1" `shouldReturn` "1"
        describe "cartesian" $ do
            it "copies" $
                executeJSONLonghand (copy :: JS String (String, String)) "1" `shouldReturn` ("1", "1")
            it "consumes" $
                executeJSONLonghand (consume :: JS String ()) "1" `shouldReturn` ()
            it "returns fst" $
                executeJSONLonghand (fst' :: JS (String, Int) String) ("1", 1) `shouldReturn` "1"
            it "returns snd" $
                executeJSONLonghand (snd' :: JS (Int, String) String) (1, "1") `shouldReturn` "1"
        describe "cocartesian" $ do
            it "injects Left" $ do
                executeJSONLonghand (injectL :: JS String (Either String ())) "1" `shouldReturn` Left "1"
            it "injects Right" $ do
                executeJSONLonghand (injectR :: JS String (Either () String)) "1" `shouldReturn` Right "1"
            describe "unify" $ do
                it "unifies Left" $
                    executeJSONLonghand (unify :: JS (Either String String) String) (Left "1") `shouldReturn` "1"
                it "unifies Right" $
                    executeJSONLonghand (unify :: JS (Either String String) String) (Right "1") `shouldReturn` "1"
            describe "tag" $ do
                it "tags Left" $
                    executeJSONLonghand (tag :: JS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
                it "tags Right" $
                    executeJSONLonghand (tag :: JS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
        describe "strong" $ do
            it "runs on first" $
                executeJSONLonghand (first' copy :: JS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
            it "runs on second" $
                executeJSONLonghand (second' copy :: JS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
        describe "choice" $ do
            describe "left'" .
                -- it "runs on left" $
                --     executeJSON (left' copy :: JS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn` (Right (Left ("1", "1")))
                it "doesn't run on right" $
                    executeJSONLonghand (left' copy :: JS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` Right 1
            describe "right'" $ do
                it "doesn't run on left" $
                    executeJSONLonghand (right' copy :: JS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
                it "runs on right" $
                    executeJSONLonghand (right' copy :: JS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
        describe "symmetric" $ do
            it "swaps" $
                executeJSONLonghand (swap :: JS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
            describe "swapEither" $ do
                it "swaps left" $
                    executeJSONLonghand (swapEither :: JS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
                it "swaps right" $
                    executeJSONLonghand (swapEither :: JS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
            it "reassocs" $
                executeJSONLonghand (reassoc :: JS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
            xdescribe "reassocEither" $ do
                it "reassocs Left" $
                    executeJSONLonghand (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                it "reassocs Right (Left)" $
                    executeJSONLonghand (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                it "reassoc Right (Right)" $
                    executeJSONLonghand (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
        describe "primitive" $ do
            describe "eq" $ do
                it "equal" $
                    executeJSONLonghand (eq :: JS (String, String) Bool) ("a", "a") `shouldReturn` True
                it "not equal" $
                    executeJSONLonghand (eq :: JS (String, String) Bool) ("a", "b") `shouldReturn` False
            it "reverses string" $
                executeJSONLonghand (reverseString :: JS String String) "abc" `shouldReturn` "cba"
        describe "primitiveconsole" $ pure ()
        describe "primitive extra" $ do
            it "converts int to string" $
                executeJSONLonghand (intToString :: JS Int String) 1 `shouldReturn` "1"
            it "concats string" $
                executeJSONLonghand (concatString :: JS (String, String) String) ("a", "b") `shouldReturn` "ab"
            it "returns const string" $ do
                executeJSONLonghand (constString "a" :: JS () String) () `shouldReturn` "a"
        describe "numeric" $ do
            it "returns const int" $ do
                executeJSONLonghand (num 1 :: JS () Int) () `shouldReturn` 1
            it "negates" $ do
                executeJSONLonghand (negate' :: JS Int Int) 1 `shouldReturn` (-1)
            it "adds" $ do
                executeJSONLonghand (add :: JS (Int, Int) Int) (1, 2) `shouldReturn` 3
            it "mults" $ do
                executeJSONLonghand (mult :: JS (Int, Int) Int) (2, 3) `shouldReturn` 6
            it "divs" $ do
                executeJSONLonghand (div' :: JS (Int, Int) Int) (4, 2) `shouldReturn` 2
            it "mods" $ do
                executeJSONLonghand (mod' :: JS (Int, Int) Int) (5, 2) `shouldReturn` 1
    xdescribe "executeJSONImports" $ do
        it "returns a string" $
            executeJSONImports (id :: JS String String) "1" `shouldReturn` "1"
        it "returns an int" $
            executeJSONImports (id :: JS Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeJSONImports (id :: JS Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeJSONImports (id :: JS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeJSONImports (id :: JS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeJSONImports (id :: JS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeJSONImports (id :: JS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeJSONImports (id :: JS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
        -- describe "bracket" .
        --     it "is idempotent" $
        --         executeJSONImports (bracket id :: JS String String) "1" `shouldReturn` "1"
        describe "category" $ do
            it "composes" $
                executeJSONImports (id :: JS String String) "1" `shouldReturn` "1"
        describe "cartesian" $ do
            it "copies" $
                executeJSONImports (copy :: JS String (String, String)) "1" `shouldReturn` ("1", "1")
            it "consumes" $
                executeJSONImports (consume :: JS String ()) "1" `shouldReturn` ()
            it "returns fst" $
                executeJSONImports (fst' :: JS (String, Int) String) ("1", 1) `shouldReturn` "1"
            it "returns snd" $
                executeJSONImports (snd' :: JS (Int, String) String) (1, "1") `shouldReturn` "1"
        describe "cocartesian" $ do
            it "injects Left" $ do
                executeJSONImports (injectL :: JS String (Either String ())) "1" `shouldReturn` Left "1"
            it "injects Right" $ do
                executeJSONImports (injectR :: JS String (Either () String)) "1" `shouldReturn` Right "1"
            describe "unify" $ do
                it "unifies Left" $
                    executeJSONImports (unify :: JS (Either String String) String) (Left "1") `shouldReturn` "1"
                it "unifies Right" $
                    executeJSONImports (unify :: JS (Either String String) String) (Right "1") `shouldReturn` "1"
            describe "tag" $ do
                it "tags Left" $
                    executeJSONImports (tag :: JS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
                it "tags Right" $
                    executeJSONImports (tag :: JS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
        describe "strong" $ do
            it "runs on first" $
                executeJSONImports (first' copy :: JS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
            it "runs on second" $
                executeJSONImports (second' copy :: JS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
        describe "choice" $ do
            describe "left'" .
                -- it "runs on left" $
                --     exeImports (left' copy :: JS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn` (Right (Left ("1", "1")))
                it "doesn't run on right" $
                    executeJSONImports (left' copy :: JS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` Right 1
            describe "right'" $ do
                it "doesn't run on left" $
                    executeJSONImports (right' copy :: JS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
                it "runs on right" $
                    executeJSONImports (right' copy :: JS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
        describe "symmetric" $ do
            it "swaps" $
                executeJSONImports (swap :: JS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
            describe "swapEither" $ do
                it "swaps left" $
                    executeJSONImports (swapEither :: JS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
                it "swaps right" $
                    executeJSONImports (swapEither :: JS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
            it "reassocs" $
                executeJSONImports (reassoc :: JS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
            xdescribe "reassocEither" $ do
                it "reassocs Left" $
                    executeJSONImports (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                it "reassocs Right (Left)" $
                    executeJSONImports (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                it "reassoc Right (Right)" $
                    executeJSONImports (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
        describe "primitive" $ do
            describe "eq" $ do
                it "equal" $
                    executeJSONImports (eq :: JS (String, String) Bool) ("a", "a") `shouldReturn` True
                it "not equal" $
                    executeJSONImports (eq :: JS (String, String) Bool) ("a", "b") `shouldReturn` False
            it "reverses string" $
                executeJSONImports (reverseString :: JS String String) "abc" `shouldReturn` "cba"
        describe "primitiveconsole" $ pure ()
        describe "primitive extra" $ do
            it "converts int to string" $
                executeJSONImports (intToString :: JS Int String) 1 `shouldReturn` "1"
            it "concats string" $
                executeJSONImports (concatString :: JS (String, String) String) ("a", "b") `shouldReturn` "ab"
            it "returns const string" $ do
                executeJSONImports (constString "a" :: JS () String) () `shouldReturn` "a"
        describe "numeric" $ do
            it "returns const int" $ do
                executeJSONImports (num 1 :: JS () Int) () `shouldReturn` 1
            it "negates" $ do
                executeJSONImports (negate' :: JS Int Int) 1 `shouldReturn` (-1)
            it "adds" $ do
                executeJSONImports (add :: JS (Int, Int) Int) (1, 2) `shouldReturn` 3
            it "mults" $ do
                executeJSONImports (mult :: JS (Int, Int) Int) (2, 3) `shouldReturn` 6
            it "divs" $ do
                executeJSONImports (div' :: JS (Int, Int) Int) (4, 2) `shouldReturn` 2
            it "mods" $ do
                executeJSONImports (mod' :: JS (Int, Int) Int) (5, 2) `shouldReturn` 1
    describe "executeJSONShorthand" $ do
        it "returns a string" $
            executeJSONShorthand (id :: JS String String) "1" `shouldReturn` "1"
        it "returns an int" $
            executeJSONShorthand (id :: JS Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeJSONShorthand (id :: JS Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeJSONShorthand (id :: JS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeJSONShorthand (id :: JS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeJSONShorthand (id :: JS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeJSONShorthand (id :: JS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeJSONShorthand (id :: JS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
        -- describe "bracket" .
        --     it "is idempotent" $
        --         executeJSONShorthand (bracket id :: JS String String) "1" `shouldReturn` "1"
        describe "category" $ do
            it "composes" $
                executeJSONShorthand (id :: JS String String) "1" `shouldReturn` "1"
        describe "cartesian" $ do
            it "copies" $
                executeJSONShorthand (copy :: JS String (String, String)) "1" `shouldReturn` ("1", "1")
            it "consumes" $
                executeJSONShorthand (consume :: JS String ()) "1" `shouldReturn` ()
            it "returns fst" $
                executeJSONShorthand (fst' :: JS (String, Int) String) ("1", 1) `shouldReturn` "1"
            it "returns snd" $
                executeJSONShorthand (snd' :: JS (Int, String) String) (1, "1") `shouldReturn` "1"
        describe "cocartesian" $ do
            it "injects Left" $ do
                executeJSONShorthand (injectL :: JS String (Either String ())) "1" `shouldReturn` Left "1"
            it "injects Right" $ do
                executeJSONShorthand (injectR :: JS String (Either () String)) "1" `shouldReturn` Right "1"
            describe "unify" $ do
                it "unifies Left" $
                    executeJSONShorthand (unify :: JS (Either String String) String) (Left "1") `shouldReturn` "1"
                it "unifies Right" $
                    executeJSONShorthand (unify :: JS (Either String String) String) (Right "1") `shouldReturn` "1"
            describe "tag" $ do
                it "tags Left" $
                    executeJSONShorthand (tag :: JS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
                it "tags Right" $
                    executeJSONShorthand (tag :: JS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
        describe "strong" $ do
            it "runs on first" $
                executeJSONShorthand (first' copy :: JS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
            it "runs on second" $
                executeJSONShorthand (second' copy :: JS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
        describe "choice" $ do
            describe "left'" .
                -- it "runs on left" $
                --     exeShorthand (left' copy :: JS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn` (Right (Left ("1", "1")))
                it "doesn't run on right" $
                    executeJSONShorthand (left' copy :: JS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` Right 1
            describe "right'" $ do
                it "doesn't run on left" $
                    executeJSONShorthand (right' copy :: JS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
                it "runs on right" $
                    executeJSONShorthand (right' copy :: JS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
        describe "symmetric" $ do
            it "swaps" $
                executeJSONShorthand (swap :: JS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
            describe "swapEither" $ do
                it "swaps left" $
                    executeJSONShorthand (swapEither :: JS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
                it "swaps right" $
                    executeJSONShorthand (swapEither :: JS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
            it "reassocs" $
                executeJSONShorthand (reassoc :: JS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
            xdescribe "reassocEither" $ do
                it "reassocs Left" $
                    executeJSONShorthand (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                it "reassocs Right (Left)" $
                    executeJSONShorthand (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                it "reassoc Right (Right)" $
                    executeJSONShorthand (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
        describe "primitive" $ do
            describe "eq" $ do
                it "equal" $
                    executeJSONShorthand (eq :: JS (String, String) Bool) ("a", "a") `shouldReturn` True
                it "not equal" $
                    executeJSONShorthand (eq :: JS (String, String) Bool) ("a", "b") `shouldReturn` False
            it "reverses string" $
                executeJSONShorthand (reverseString :: JS String String) "abc" `shouldReturn` "cba"
        describe "primitiveconsole" $ pure ()
        describe "primitive extra" $ do
            it "converts int to string" $
                executeJSONShorthand (intToString :: JS Int String) 1 `shouldReturn` "1"
            it "concats string" $
                executeJSONShorthand (concatString :: JS (String, String) String) ("a", "b") `shouldReturn` "ab"
            it "returns const string" $ do
                executeJSONShorthand (constString "a" :: JS () String) () `shouldReturn` "a"
        describe "numeric" $ do
            it "returns const int" $ do
                executeJSONShorthand (num 1 :: JS () Int) () `shouldReturn` 1
            it "negates" $ do
                executeJSONShorthand (negate' :: JS Int Int) 1 `shouldReturn` (-1)
            it "adds" $ do
                executeJSONShorthand (add :: JS (Int, Int) Int) (1, 2) `shouldReturn` 3
            it "mults" $ do
                executeJSONShorthand (mult :: JS (Int, Int) Int) (2, 3) `shouldReturn` 6
            it "divs" $ do
                executeJSONShorthand (div' :: JS (Int, Int) Int) (4, 2) `shouldReturn` 2
            it "mods" $ do
                executeJSONShorthand (mod' :: JS (Int, Int) Int) (5, 2) `shouldReturn` 1
