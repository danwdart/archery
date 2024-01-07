{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main (main) where

-- import Control.Category.Interpret
import Data.Aeson                            qualified as A
-- import Data.ByteString.Char8      qualified as BS
import Data.ByteString.Lazy.Char8            qualified as BSL
import Data.Code.Haskell
-- import Data.Code.JS
-- import Data.Code.PHP
import Data.Function.AskName
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.HelloWorld
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
-- import Data.Person
import Data.Prims
import Data.Render.File.WithDefinitions
import Data.Render.File.WithImports
import Data.Render.File.WithShorthand
import Data.Render.Statement.WithDefinitions
import Data.Render.Statement.WithShorthand
-- import Data.Person
import Data.Yaml                             qualified as Y
import System.Directory

main ∷ IO ()
main = do
    removeDirectoryRecursive "data/examples"

    -- removeDirectoryRecursive "data/examples/jcat"

    createDirectoryIfMissing True "data/examples/jcat"

    A.encodeFile "data/examples/jcat/askName.json" (askName :: FreeFunc Prims () ())
    A.encodeFile "data/examples/jcat/collatzStep.json" (collatzStep :: FreeFunc Prims Int Int)
    A.encodeFile "data/examples/jcat/helloWorld.json" (helloWorld :: FreeFunc Prims () ())
    A.encodeFile "data/examples/jcat/isPalindrome.json" (isPalindrome :: FreeFunc Prims String Bool)
    -- A.encodeFile "data/examples/jcat/greetData.json" (greetData :: FreeFunc Prims Person String)
    A.encodeFile "data/examples/jcat/greetTuple.json" (greetTuple :: FreeFunc Prims (String, Int) String)
    A.encodeFile "data/examples/jcat/reverseInput.json" (revInputProgram :: FreeFunc Prims () ())

    createDirectoryIfMissing True "data/examples/ycat"

    Y.encodeFile "data/examples/ycat/askName.yaml" (askName :: FreeFunc Prims () ())
    Y.encodeFile "data/examples/ycat/collatzStep.yaml" (collatzStep :: FreeFunc Prims Int Int)
    Y.encodeFile "data/examples/ycat/helloWorld.yaml" (helloWorld :: FreeFunc Prims () ())
    Y.encodeFile "data/examples/ycat/isPalindrome.yaml" (isPalindrome :: FreeFunc Prims String Bool)
    -- Y.encodeFile "data/examples/ycat/greetData.yaml" (greetData :: FreeFunc Prims Person String)
    Y.encodeFile "data/examples/ycat/greetTuple.yaml" (greetTuple :: FreeFunc Prims (String, Int) String)
    Y.encodeFile "data/examples/ycat/reverseInput.yaml" (revInputProgram :: FreeFunc Prims () ())

    -- removeDirectoryRecursive "data/examples/statements"

    createDirectoryIfMissing True "data/examples/statements/definitions/haskell"

    BSL.writeFile "data/examples/statements/definitions/haskell/askName.hs" $ renderStatementWithDefinitions (askName :: HS () ())
    BSL.writeFile "data/examples/statements/definitions/haskell/collatzStep.hs" $ renderStatementWithDefinitions (collatzStep :: HS Int Int)
    BSL.writeFile "data/examples/statements/definitions/haskell/helloWorld.hs" $ renderStatementWithDefinitions (helloWorld :: HS () ())
    BSL.writeFile "data/examples/statements/definitions/haskell/isPalindrome.hs" $ renderStatementWithDefinitions (isPalindrome :: HS String Bool)
    BSL.writeFile "data/examples/statements/definitions/haskell/greetTuple.hs" $ renderStatementWithDefinitions (greetTuple :: HS (String, Int) String)
    BSL.writeFile "data/examples/statements/definitions/haskell/reverseInput.hs" $ renderStatementWithDefinitions (revInputProgram :: HS () ())

    createDirectoryIfMissing True "data/examples/statements/shorthand/haskell"

    BSL.writeFile "data/examples/statements/shorthand/haskell/askName.hs" $ renderStatementWithShorthand (askName :: HS () ())
    BSL.writeFile "data/examples/statements/shorthand/haskell/collatzStep.hs" $ renderStatementWithShorthand (collatzStep :: HS Int Int)
    BSL.writeFile "data/examples/statements/shorthand/haskell/helloWorld.hs" $ renderStatementWithShorthand (helloWorld :: HS () ())
    BSL.writeFile "data/examples/statements/shorthand/haskell/isPalindrome.hs" $ renderStatementWithShorthand (isPalindrome :: HS String Bool)
    BSL.writeFile "data/examples/statements/shorthand/haskell/greetTuple.hs" $ renderStatementWithShorthand (greetTuple :: HS (String, Int) String)
    BSL.writeFile "data/examples/statements/definitions/haskell/reverseInput.hs" $ renderStatementWithShorthand (revInputProgram :: HS () ())

    -- createDirectoryIfMissing True "data/examples/statements/js"
--
    -- BSL.writeFile "data/examples/statements/js/collatzStep.js" $ renderStatement (collatzStep :: JS Int Int)
    -- BSL.writeFile "data/examples/statements/js/isPalindrome.js" $ renderStatement (isPalindrome :: JS String Bool)
    -- BSL.writeFile "data/examples/statements/js/greetTuple.js" $ renderStatement (greetTuple :: JS (String, Int) String)
    -- BSL.writeFile "data/examples/statements/js/reverseInput.js" $ renderStatement (revInputProgram :: JS () ())
--
    -- createDirectoryIfMissing True "data/examples/statements/php"
--
    -- BSL.writeFile "data/examples/statements/php/collatzStep.php" $ renderStatement (collatzStep :: PHP Int Int)
    -- BSL.writeFile "data/examples/statements/php/isPalindrome.php" $ renderStatement (isPalindrome :: PHP String Bool)
    -- BSL.writeFile "data/examples/statements/php/greetTuple.php" $ renderStatement (greetTuple :: PHP (String, Int) String)
    -- BSL.writeFile "data/examples/statements/php/reverseInput.php" $ renderStatement (revInputProgram :: PHP () ())

    -- removeDirectoryRecursive "data/examples/programs"

    createDirectoryIfMissing True "data/examples/programs/withimports/haskell"

    BSL.writeFile "data/examples/programs/withimports/haskell/askName.hs" $ renderFileWithImports (askName :: HS () ())
    BSL.writeFile "data/examples/programs/withimports/haskell/collatzStep.hs" $ renderFileWithImports (collatzStep :: HS Int Int)
    BSL.writeFile "data/examples/programs/withimports/haskell/helloWorld.hs" $ renderFileWithImports (helloWorld :: HS () ())
    BSL.writeFile "data/examples/programs/withimports/haskell/isPalindrome.hs" $ renderFileWithImports (isPalindrome :: HS String Bool)
    BSL.writeFile "data/examples/programs/withimports/haskell/greetTuple.hs" $ renderFileWithImports (greetTuple :: HS (String, Int) String)
    BSL.writeFile "data/examples/programs/withimports/haskell/reverseInput.hs" $ renderFileWithImports (revInputProgram :: HS () ())

    createDirectoryIfMissing True "data/examples/programs/withdefinitions/haskell"

    BSL.writeFile "data/examples/programs/withdefinitions/haskell/askName.hs" $ renderFileWithDefinitions (askName :: HS () ())
    BSL.writeFile "data/examples/programs/withdefinitions/haskell/collatzStep.hs" $ renderFileWithDefinitions (collatzStep :: HS Int Int)
    BSL.writeFile "data/examples/programs/withdefinitions/haskell/helloWorld.hs" $ renderFileWithDefinitions (helloWorld :: HS () ())
    BSL.writeFile "data/examples/programs/withdefinitions/haskell/isPalindrome.hs" $ renderFileWithDefinitions (isPalindrome :: HS String Bool)
    BSL.writeFile "data/examples/programs/withdefinitions/haskell/greetTuple.hs" $ renderFileWithDefinitions (greetTuple :: HS (String, Int) String)
    BSL.writeFile "data/examples/programs/withdefinitions/haskell/reverseInput.hs" $ renderFileWithDefinitions (revInputProgram :: HS () ())

    createDirectoryIfMissing True "data/examples/programs/withshorthand/haskell"

    BSL.writeFile "data/examples/programs/withshorthand/haskell/askName.hs" $ renderFileWithShorthand (askName :: HS () ())
    BSL.writeFile "data/examples/programs/withshorthand/haskell/collatzStep.hs" $ renderFileWithShorthand (collatzStep :: HS Int Int)
    BSL.writeFile "data/examples/programs/withshorthand/haskell/helloWorld.hs" $ renderFileWithShorthand (helloWorld :: HS () ())
    BSL.writeFile "data/examples/programs/withshorthand/haskell/isPalindrome.hs" $ renderFileWithShorthand (isPalindrome :: HS String Bool)
    BSL.writeFile "data/examples/programs/withshorthand/haskell/greetTuple.hs" $ renderFileWithShorthand (greetTuple :: HS (String, Int) String)
    BSL.writeFile "data/examples/programs/withshorthand/haskell/reverseInput.hs" $ renderFileWithShorthand (revInputProgram :: HS () ())

    -- createDirectoryIfMissing True "data/examples/programs/js/JS"
--
    -- BSL.writeFile "data/examples/programs/js/JS/collatzStep.js" $ renderFileWithImports (collatzStep :: JS Int Int)
    -- BSL.writeFile "data/examples/programs/js/JS/isPalindrome.js" $ renderFileWithImports (isPalindrome :: JS String Bool)
    -- BSL.writeFile "data/examples/programs/js/JS/greetTuple.js" $ renderFileWithImports (greetTuple :: JS (String, Int) String)
    -- BSL.writeFile "data/examples/programs/js/JS/reverseInput.js" $ renderFileWithImports (revInputProgram :: JS () ())
