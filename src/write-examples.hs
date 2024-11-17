{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main (main) where

-- import Control.Category.Interpret
import Data.Aeson                            qualified as A
-- import Data.ByteString.Char8      qualified as BS
import Data.ByteString.Lazy.Char8            qualified as BSL
import Data.Code.Haskell
import Data.Code.JS
import Data.Code.PHP
import Data.Code.TS
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

    do
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

    {-}
    do
        createDirectoryIfMissing True "data/examples/statements/definitions/js"

        BSL.writeFile "data/examples/statements/definitions/js/askName.js" $ renderStatementWithDefinitions (askName :: JS () ())
        BSL.writeFile "data/examples/statements/definitions/js/collatzStep.js" $ renderStatementWithDefinitions (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/statements/definitions/js/helloWorld.js" $ renderStatementWithDefinitions (helloWorld :: JS () ())
        BSL.writeFile "data/examples/statements/definitions/js/isPalindrome.js" $ renderStatementWithDefinitions (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/statements/definitions/js/greetTuple.js" $ renderStatementWithDefinitions (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/statements/definitions/js/reverseInput.js" $ renderStatementWithDefinitions (revInputProgram :: JS () ())

        createDirectoryIfMissing True "data/examples/statements/shorthand/js"

        BSL.writeFile "data/examples/statements/shorthand/js/askName.js" $ renderStatementWithShorthand (askName :: JS () ())
        BSL.writeFile "data/examples/statements/shorthand/js/collatzStep.js" $ renderStatementWithShorthand (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/statements/shorthand/js/helloWorld.js" $ renderStatementWithShorthand (helloWorld :: JS () ())
        BSL.writeFile "data/examples/statements/shorthand/js/isPalindrome.js" $ renderStatementWithShorthand (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/statements/shorthand/js/greetTuple.js" $ renderStatementWithShorthand (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/statements/shorthand/js/reverseInput.js" $ renderStatementWithShorthand (revInputProgram :: JS () ())

        createDirectoryIfMissing True "data/examples/programs/withimports/js"

        BSL.writeFile "data/examples/programs/withimports/js/askName.js" $ renderFileWithImports (askName :: JS () ())
        BSL.writeFile "data/examples/programs/withimports/js/collatzStep.js" $ renderFileWithImports (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/programs/withimports/js/helloWorld.js" $ renderFileWithImports (helloWorld :: JS () ())
        BSL.writeFile "data/examples/programs/withimports/js/isPalindrome.js" $ renderFileWithImports (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/programs/withimports/js/greetTuple.js" $ renderFileWithImports (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/programs/withimports/js/reverseInput.js" $ renderFileWithImports (revInputProgram :: JS () ())

        createDirectoryIfMissing True "data/examples/programs/withdefinitions/js"

        BSL.writeFile "data/examples/programs/withdefinitions/js/askName.js" $ renderFileWithDefinitions (askName :: JS () ())
        BSL.writeFile "data/examples/programs/withdefinitions/js/collatzStep.js" $ renderFileWithDefinitions (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/programs/withdefinitions/js/helloWorld.js" $ renderFileWithDefinitions (helloWorld :: JS () ())
        BSL.writeFile "data/examples/programs/withdefinitions/js/isPalindrome.js" $ renderFileWithDefinitions (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/programs/withdefinitions/js/greetTuple.js" $ renderFileWithDefinitions (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/programs/withdefinitions/js/reverseInput.js" $ renderFileWithDefinitions (revInputProgram :: JS () ())

        createDirectoryIfMissing True "data/examples/programs/witjshorthand/js"

        BSL.writeFile "data/examples/programs/withshorthand/js/askName.js" $ renderFileWithShorthand (askName :: JS () ())
        BSL.writeFile "data/examples/programs/withshorthand/js/collatzStep.js" $ renderFileWithShorthand (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/programs/withshorthand/js/helloWorld.js" $ renderFileWithShorthand (helloWorld :: JS () ())
        BSL.writeFile "data/examples/programs/withshorthand/js/isPalindrome.js" $ renderFileWithShorthand (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/programs/withshorthand/js/greetTuple.js" $ renderFileWithShorthand (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/programs/withshorthand/js/reverseInput.js" $ renderFileWithShorthand (revInputProgram :: JS () ())

    do
        createDirectoryIfMissing True "data/examples/statements/definitions/ts"

        BSL.writeFile "data/examples/statements/definitions/ts/askName.ts" $ renderStatementWithDefinitions (askName :: TS () ())
        BSL.writeFile "data/examples/statements/definitions/ts/collatzStep.ts" $ renderStatementWithDefinitions (collatzStep :: TS Int Int)
        BSL.writeFile "data/examples/statements/definitions/ts/helloWorld.ts" $ renderStatementWithDefinitions (helloWorld :: TS () ())
        BSL.writeFile "data/examples/statements/definitions/ts/isPalindrome.ts" $ renderStatementWithDefinitions (isPalindrome :: TS String Bool)
        BSL.writeFile "data/examples/statements/definitions/ts/greetTuple.ts" $ renderStatementWithDefinitions (greetTuple :: TS (String, Int) String)
        BSL.writeFile "data/examples/statements/definitions/ts/reverseInput.ts" $ renderStatementWithDefinitions (revInputProgram :: TS () ())

        createDirectoryIfMissing True "data/examples/statements/shorthand/ts"

        BSL.writeFile "data/examples/statements/shorthand/ts/askName.ts" $ renderStatementWithShorthand (askName :: TS () ())
        BSL.writeFile "data/examples/statements/shorthand/ts/collatzStep.ts" $ renderStatementWithShorthand (collatzStep :: TS Int Int)
        BSL.writeFile "data/examples/statements/shorthand/ts/helloWorld.ts" $ renderStatementWithShorthand (helloWorld :: TS () ())
        BSL.writeFile "data/examples/statements/shorthand/ts/isPalindrome.ts" $ renderStatementWithShorthand (isPalindrome :: TS String Bool)
        BSL.writeFile "data/examples/statements/shorthand/ts/greetTuple.ts" $ renderStatementWithShorthand (greetTuple :: TS (String, Int) String)
        BSL.writeFile "data/examples/statements/shorthand/ts/reverseInput.ts" $ renderStatementWithShorthand (revInputProgram :: TS () ())

        createDirectoryIfMissing True "data/examples/programs/withimports/ts"

        BSL.writeFile "data/examples/programs/withimports/ts/askName.ts" $ renderFileWithImports (askName :: TS () ())
        BSL.writeFile "data/examples/programs/withimports/ts/collatzStep.ts" $ renderFileWithImports (collatzStep :: TS Int Int)
        BSL.writeFile "data/examples/programs/withimports/ts/helloWorld.ts" $ renderFileWithImports (helloWorld :: TS () ())
        BSL.writeFile "data/examples/programs/withimports/ts/isPalindrome.ts" $ renderFileWithImports (isPalindrome :: TS String Bool)
        BSL.writeFile "data/examples/programs/withimports/ts/greetTuple.ts" $ renderFileWithImports (greetTuple :: TS (String, Int) String)
        BSL.writeFile "data/examples/programs/withimports/ts/reverseInput.ts" $ renderFileWithImports (revInputProgram :: TS () ())

        createDirectoryIfMissing True "data/examples/programs/withdefinitions/ts"

        BSL.writeFile "data/examples/programs/withdefinitions/ts/askName.ts" $ renderFileWithDefinitions (askName :: TS () ())
        BSL.writeFile "data/examples/programs/withdefinitions/ts/collatzStep.ts" $ renderFileWithDefinitions (collatzStep :: TS Int Int)
        BSL.writeFile "data/examples/programs/withdefinitions/ts/helloWorld.ts" $ renderFileWithDefinitions (helloWorld :: TS () ())
        BSL.writeFile "data/examples/programs/withdefinitions/ts/isPalindrome.ts" $ renderFileWithDefinitions (isPalindrome :: TS String Bool)
        BSL.writeFile "data/examples/programs/withdefinitions/ts/greetTuple.ts" $ renderFileWithDefinitions (greetTuple :: TS (String, Int) String)
        BSL.writeFile "data/examples/programs/withdefinitions/ts/reverseInput.ts" $ renderFileWithDefinitions (revInputProgram :: TS () ())

        createDirectoryIfMissing True "data/examples/programs/witjshorthand/ts"

        BSL.writeFile "data/examples/programs/withshorthand/ts/askName.ts" $ renderFileWithShorthand (askName :: TS () ())
        BSL.writeFile "data/examples/programs/withshorthand/ts/collatzStep.ts" $ renderFileWithShorthand (collatzStep :: TS Int Int)
        BSL.writeFile "data/examples/programs/withshorthand/ts/helloWorld.ts" $ renderFileWithShorthand (helloWorld :: TS () ())
        BSL.writeFile "data/examples/programs/withshorthand/ts/isPalindrome.ts" $ renderFileWithShorthand (isPalindrome :: TS String Bool)
        BSL.writeFile "data/examples/programs/withshorthand/ts/greetTuple.ts" $ renderFileWithShorthand (greetTuple :: TS (String, Int) String)
        BSL.writeFile "data/examples/programs/withshorthand/ts/reverseInput.ts" $ renderFileWithShorthand (revInputProgram :: TS () ())

    do
        createDirectoryIfMissing True "data/examples/statements/definitions/php"

        BSL.writeFile "data/examples/statements/definitions/php/askName.php" $ renderStatementWithDefinitions (askName :: PHP () ())
        BSL.writeFile "data/examples/statements/definitions/php/collatzStep.php" $ renderStatementWithDefinitions (collatzStep :: PHP Int Int)
        BSL.writeFile "data/examples/statements/definitions/php/helloWorld.php" $ renderStatementWithDefinitions (helloWorld :: PHP () ())
        BSL.writeFile "data/examples/statements/definitions/php/isPalindrome.php" $ renderStatementWithDefinitions (isPalindrome :: PHP String Bool)
        BSL.writeFile "data/examples/statements/definitions/php/greetTuple.php" $ renderStatementWithDefinitions (greetTuple :: PHP (String, Int) String)
        BSL.writeFile "data/examples/statements/definitions/php/reverseInput.php" $ renderStatementWithDefinitions (revInputProgram :: PHP () ())

        createDirectoryIfMissing True "data/examples/statements/shorthand/php"

        BSL.writeFile "data/examples/statements/shorthand/php/askName.php" $ renderStatementWithShorthand (askName :: PHP () ())
        BSL.writeFile "data/examples/statements/shorthand/php/collatzStep.php" $ renderStatementWithShorthand (collatzStep :: PHP Int Int)
        BSL.writeFile "data/examples/statements/shorthand/php/helloWorld.php" $ renderStatementWithShorthand (helloWorld :: PHP () ())
        BSL.writeFile "data/examples/statements/shorthand/php/isPalindrome.php" $ renderStatementWithShorthand (isPalindrome :: PHP String Bool)
        BSL.writeFile "data/examples/statements/shorthand/php/greetTuple.php" $ renderStatementWithShorthand (greetTuple :: PHP (String, Int) String)
        BSL.writeFile "data/examples/statements/shorthand/php/reverseInput.php" $ renderStatementWithShorthand (revInputProgram :: PHP () ())

        createDirectoryIfMissing True "data/examples/programs/withimports/php"

        BSL.writeFile "data/examples/programs/withimports/php/askName.php" $ renderFileWithImports (askName :: PHP () ())
        BSL.writeFile "data/examples/programs/withimports/php/collatzStep.php" $ renderFileWithImports (collatzStep :: PHP Int Int)
        BSL.writeFile "data/examples/programs/withimports/php/helloWorld.php" $ renderFileWithImports (helloWorld :: PHP () ())
        BSL.writeFile "data/examples/programs/withimports/php/isPalindrome.php" $ renderFileWithImports (isPalindrome :: PHP String Bool)
        BSL.writeFile "data/examples/programs/withimports/php/greetTuple.php" $ renderFileWithImports (greetTuple :: PHP (String, Int) String)
        BSL.writeFile "data/examples/programs/withimports/php/reverseInput.php" $ renderFileWithImports (revInputProgram :: PHP () ())

        createDirectoryIfMissing True "data/examples/programs/withdefinitions/php"

        BSL.writeFile "data/examples/programs/withdefinitions/php/askName.php" $ renderFileWithDefinitions (askName :: PHP () ())
        BSL.writeFile "data/examples/programs/withdefinitions/php/collatzStep.php" $ renderFileWithDefinitions (collatzStep :: PHP Int Int)
        BSL.writeFile "data/examples/programs/withdefinitions/php/helloWorld.php" $ renderFileWithDefinitions (helloWorld :: PHP () ())
        BSL.writeFile "data/examples/programs/withdefinitions/php/isPalindrome.php" $ renderFileWithDefinitions (isPalindrome :: PHP String Bool)
        BSL.writeFile "data/examples/programs/withdefinitions/php/greetTuple.php" $ renderFileWithDefinitions (greetTuple :: PHP (String, Int) String)
        BSL.writeFile "data/examples/programs/withdefinitions/php/reverseInput.php" $ renderFileWithDefinitions (revInputProgram :: PHP () ())

        createDirectoryIfMissing True "data/examples/programs/witjshorthand/php"

        BSL.writeFile "data/examples/programs/withshorthand/php/askName.php" $ renderFileWithShorthand (askName :: PHP () ())
        BSL.writeFile "data/examples/programs/withshorthand/php/collatzStep.php" $ renderFileWithShorthand (collatzStep :: PHP Int Int)
        BSL.writeFile "data/examples/programs/withshorthand/php/helloWorld.php" $ renderFileWithShorthand (helloWorld :: PHP () ())
        BSL.writeFile "data/examples/programs/withshorthand/php/isPalindrome.php" $ renderFileWithShorthand (isPalindrome :: PHP String Bool)
        BSL.writeFile "data/examples/programs/withshorthand/php/greetTuple.php" $ renderFileWithShorthand (greetTuple :: PHP (String, Int) String)
        BSL.writeFile "data/examples/programs/withshorthand/php/reverseInput.php" $ renderFileWithShorthand (revInputProgram :: PHP () ())
    -}