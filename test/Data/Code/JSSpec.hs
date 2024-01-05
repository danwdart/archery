module Data.Code.JSSpec where
{-}
import Control.Category
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.JSON.WithDefinitions
import Control.Category.Execute.JSON.WithImports
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
-}
import Test.Hspec                         hiding (runIO)
{-}
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
-}
spec ∷ Spec
spec = pure ()

{-} describe "JS" $ do
    
    describe "executeViaJSONWithDefinitions" $ do
        it "returns a string" $
            executeViaJSONWithDefinitions (id :: JS String String) "1" `shouldReturn` "1"
        it "returns an int" $
            executeViaJSONWithDefinitions (id :: JS Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeViaJSONWithDefinitions (id :: JS Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeViaJSONWithDefinitions (id :: JS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeViaJSONWithDefinitions (id :: JS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeViaJSONWithDefinitions (id :: JS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeViaJSONWithDefinitions (id :: JS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeViaJSONWithDefinitions (id :: JS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
        describe "bracket" .
            it "is idempotent" $
                executeViaJSONWithDefinitions (bracket id :: JS String String) "1" `shouldReturn` "1"
        describe "category" $ do
            it "composes" $
                executeViaJSONWithDefinitions (id :: JS String String) "1" `shouldReturn` "1"
        describe "cartesian" $ do
            it "copies" $
                executeViaJSONWithDefinitions (copy :: JS String (String, String)) "1" `shouldReturn` ("1", "1")
            it "consumes" $
                executeViaJSONWithDefinitions (consume :: JS String ()) "1" `shouldReturn` ()
            it "returns fst" $
                executeViaJSONWithDefinitions (fst' :: JS (String, Int) String) ("1", 1) `shouldReturn` "1"
            it "returns snd" $
                executeViaJSONWithDefinitions (snd' :: JS (Int, String) String) (1, "1") `shouldReturn` "1"
        describe "cocartesian" $ do
            it "injects Left" $ do
                executeViaJSONWithDefinitions (injectL :: JS String (Either String ())) "1" `shouldReturn` Left "1"
            it "injects Right" $ do
                executeViaJSONWithDefinitions (injectR :: JS String (Either () String)) "1" `shouldReturn` Right "1"
            describe "unify" $ do
                it "unifies Left" $
                    executeViaJSONWithDefinitions (unify :: JS (Either String String) String) (Left "1") `shouldReturn` "1"
                it "unifies Right" $
                    executeViaJSONWithDefinitions (unify :: JS (Either String String) String) (Right "1") `shouldReturn` "1"
            describe "tag" $ do
                it "tags Left" $
                    executeViaJSONWithDefinitions (tag :: JS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
                it "tags Right" $
                    executeViaJSONWithDefinitions (tag :: JS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
        describe "strong" $ do
            it "runs on first" $
                executeViaJSONWithDefinitions (first' copy :: JS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
            it "runs on second" $
                executeViaJSONWithDefinitions (second' copy :: JS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
        describe "choice" $ do
            describe "left'" .
                -- it "runs on left" $
                --     executeViaJSON (left' copy :: JS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn` (Right (Left ("1", "1")))
                it "doesn't run on right" $
                    executeViaJSONWithDefinitions (left' copy :: JS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` Right 1
            describe "right'" $ do
                it "doesn't run on left" $
                    executeViaJSONWithDefinitions (right' copy :: JS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
                it "runs on right" $
                    executeViaJSONWithDefinitions (right' copy :: JS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
        describe "symmetric" $ do
            it "swaps" $
                executeViaJSONWithDefinitions (swap :: JS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
            describe "swapEither" $ do
                it "swaps left" $
                    executeViaJSONWithDefinitions (swapEither :: JS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
                it "swaps right" $
                    executeViaJSONWithDefinitions (swapEither :: JS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
            it "reassocs" $
                executeViaJSONWithDefinitions (reassoc :: JS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
            xdescribe "reassocEither" $ do
                it "reassocs Left" $
                    executeViaJSONWithDefinitions (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                it "reassocs Right (Left)" $
                    executeViaJSONWithDefinitions (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                it "reassoc Right (Right)" $
                    executeViaJSONWithDefinitions (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
        describe "primitive" $ do
            describe "eq" $ do
                it "equal" $
                    executeViaJSONWithDefinitions (eq :: JS (String, String) Bool) ("a", "a") `shouldReturn` True
                it "not equal" $
                    executeViaJSONWithDefinitions (eq :: JS (String, String) Bool) ("a", "b") `shouldReturn` False
            it "reverses string" $
                executeViaJSONWithDefinitions (reverseString :: JS String String) "abc" `shouldReturn` "cba"
        describe "primitiveconsole" $ pure ()
        describe "primitive extra" $ do
            it "converts int to string" $
                executeViaJSONWithDefinitions (intToString :: JS Int String) 1 `shouldReturn` "1"
            it "concats string" $
                executeViaJSONWithDefinitions (concatString :: JS (String, String) String) ("a", "b") `shouldReturn` "ab"
            it "returns const string" $ do
                executeViaJSONWithDefinitions (constString "a" :: JS () String) () `shouldReturn` "a"
        describe "numeric" $ do
            it "returns const int" $ do
                executeViaJSONWithDefinitions (num 1 :: JS () Int) () `shouldReturn` 1
            it "negates" $ do
                executeViaJSONWithDefinitions (negate' :: JS Int Int) 1 `shouldReturn` (-1)
            it "adds" $ do
                executeViaJSONWithDefinitions (add :: JS (Int, Int) Int) (1, 2) `shouldReturn` 3
            it "mults" $ do
                executeViaJSONWithDefinitions (mult :: JS (Int, Int) Int) (2, 3) `shouldReturn` 6
            it "divs" $ do
                executeViaJSONWithDefinitions (div' :: JS (Int, Int) Int) (4, 2) `shouldReturn` 2
            it "mods" $ do
                executeViaJSONWithDefinitions (mod' :: JS (Int, Int) Int) (5, 2) `shouldReturn` 1
    describe "executeViaJSONWithImports" $ do
        it "returns a string" $
            executeViaJSONWithImports (id :: JS String String) "1" `shouldReturn` "1"
        it "returns an int" $
            executeViaJSONWithImports (id :: JS Int Int) 1 `shouldReturn` 1
        it "returns a bool" $
            executeViaJSONWithImports (id :: JS Bool Bool) True `shouldReturn` True
        it "returns a tuple" $
            executeViaJSONWithImports (id :: JS (String, Int) (String, Int)) ("1", 1) `shouldReturn` ("1", 1)
        describe "Either" $ do
            it "returns a Left" $
                executeViaJSONWithImports (id :: JS (Either String Int) (Either String Int)) (Left "1") `shouldReturn` Left "1"
            it "returns a Right" $
                executeViaJSONWithImports (id :: JS (Either String Int) (Either String Int)) (Right 1) `shouldReturn` Right 1
        describe "Maybe" $ do
            it "returns a Nothing" $
                executeViaJSONWithImports (id :: JS (Maybe Int) (Maybe Int)) Nothing `shouldReturn` Nothing
            it "returns a Just" $
                executeViaJSONWithImports (id :: JS (Maybe Int) (Maybe Int)) (Just 1) `shouldReturn` Just 1
        describe "bracket" .
            it "is idempotent" $
                executeViaJSONWithImports (bracket id :: JS String String) "1" `shouldReturn` "1"
        describe "category" $ do
            it "composes" $
                executeViaJSONWithImports (id :: JS String String) "1" `shouldReturn` "1"
        describe "cartesian" $ do
            it "copies" $
                executeViaJSONWithImports (copy :: JS String (String, String)) "1" `shouldReturn` ("1", "1")
            it "consumes" $
                executeViaJSONWithImports (consume :: JS String ()) "1" `shouldReturn` ()
            it "returns fst" $
                executeViaJSONWithImports (fst' :: JS (String, Int) String) ("1", 1) `shouldReturn` "1"
            it "returns snd" $
                executeViaJSONWithImports (snd' :: JS (Int, String) String) (1, "1") `shouldReturn` "1"
        describe "cocartesian" $ do
            it "injects Left" $ do
                executeViaJSONWithImports (injectL :: JS String (Either String ())) "1" `shouldReturn` Left "1"
            it "injects Right" $ do
                executeViaJSONWithImports (injectR :: JS String (Either () String)) "1" `shouldReturn` Right "1"
            describe "unify" $ do
                it "unifies Left" $
                    executeViaJSONWithImports (unify :: JS (Either String String) String) (Left "1") `shouldReturn` "1"
                it "unifies Right" $
                    executeViaJSONWithImports (unify :: JS (Either String String) String) (Right "1") `shouldReturn` "1"
            describe "tag" $ do
                it "tags Left" $
                    executeViaJSONWithImports (tag :: JS (Bool, String) (Either String String)) (False, "1") `shouldReturn` Left "1"
                it "tags Right" $
                    executeViaJSONWithImports (tag :: JS (Bool, String) (Either String String)) (True, "1") `shouldReturn` Right "1"
        describe "strong" $ do
            it "runs on first" $
                executeViaJSONWithImports (first' copy :: JS (String, String) ((String, String), String)) ("1", "2") `shouldReturn` (("1", "1"), "2")
            it "runs on second" $
                executeViaJSONWithImports (second' copy :: JS (String, String) (String, (String, String))) ("1", "2") `shouldReturn` ("1", ("2", "2"))
        describe "choice" $ do
            describe "left'" .
                -- it "runs on left" $
                --     exeImports (left' copy :: JS (Either String Int) (Either (String, String) Int)) (Left "1") `shouldReturn` (Right (Left ("1", "1")))
                it "doesn't run on right" $
                    executeViaJSONWithImports (left' copy :: JS (Either String Int) (Either (String, String) Int)) (Right 1) `shouldReturn` Right 1
            describe "right'" $ do
                it "doesn't run on left" $
                    executeViaJSONWithImports (right' copy :: JS (Either String Int) (Either String (Int, Int))) (Left "1") `shouldReturn` Left "1"
                it "runs on right" $
                    executeViaJSONWithImports (right' copy :: JS (Either String Int) (Either String (Int, Int))) (Right 1) `shouldReturn` Right (1, 1)
        describe "symmetric" $ do
            it "swaps" $
                executeViaJSONWithImports (swap :: JS (String, Int) (Int, String)) ("1", 1) `shouldReturn` (1, "1")
            describe "swapEither" $ do
                it "swaps left" $
                    executeViaJSONWithImports (swapEither :: JS (Either String String) (Either String String)) (Left "1") `shouldReturn` Right "1"
                it "swaps right" $
                    executeViaJSONWithImports (swapEither :: JS (Either String String) (Either String String)) (Right "1") `shouldReturn` Left "1"
            it "reassocs" $
                executeViaJSONWithImports (reassoc :: JS (String, (Int, Bool)) ((String, Int), Bool)) ("1", (1, True)) `shouldReturn` (("1", 1), True)
            xdescribe "reassocEither" $ do
                it "reassocs Left" $
                    executeViaJSONWithImports (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Left "1") `shouldReturn` Left (Left "1")
                it "reassocs Right (Left)" $
                    executeViaJSONWithImports (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Left 1)) `shouldReturn` Left (Right 1)
                it "reassoc Right (Right)" $
                    executeViaJSONWithImports (reassocEither :: JS (Either String (Either Int Bool)) (Either (Either String Int) Bool)) (Right (Right True)) `shouldReturn` Right True
        describe "primitive" $ do
            describe "eq" $ do
                it "equal" $
                    executeViaJSONWithImports (eq :: JS (String, String) Bool) ("a", "a") `shouldReturn` True
                it "not equal" $
                    executeViaJSONWithImports (eq :: JS (String, String) Bool) ("a", "b") `shouldReturn` False
            it "reverses string" $
                executeViaJSONWithImports (reverseString :: JS String String) "abc" `shouldReturn` "cba"
        describe "primitiveconsole" $ pure ()
        describe "primitive extra" $ do
            it "converts int to string" $
                executeViaJSONWithImports (intToString :: JS Int String) 1 `shouldReturn` "1"
            it "concats string" $
                executeViaJSONWithImports (concatString :: JS (String, String) String) ("a", "b") `shouldReturn` "ab"
            it "returns const string" $ do
                executeViaJSONWithImports (constString "a" :: JS () String) () `shouldReturn` "a"
        describe "numeric" $ do
            it "returns const int" $ do
                executeViaJSONWithImports (num 1 :: JS () Int) () `shouldReturn` 1
            it "negates" $ do
                executeViaJSONWithImports (negate' :: JS Int Int) 1 `shouldReturn` (-1)
            it "adds" $ do
                executeViaJSONWithImports (add :: JS (Int, Int) Int) (1, 2) `shouldReturn` 3
            it "mults" $ do
                executeViaJSONWithImports (mult :: JS (Int, Int) Int) (2, 3) `shouldReturn` 6
            it "divs" $ do
                executeViaJSONWithImports (div' :: JS (Int, Int) Int) (4, 2) `shouldReturn` 2
            it "mods" $ do
                executeViaJSONWithImports (mod' :: JS (Int, Int) Int) (5, 2) `shouldReturn` 1
-}