{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wwarn #-}

module Main (main) where

-- import Control.Category.Interpret
import Data.Aeson                            qualified as A
-- import Data.ByteString.Char8      qualified as BS
import Data.ByteString.Lazy.Char8            qualified as BSL
import Data.Code.Haskell
-- import Data.Code.JS
-- import Data.Code.PHP
-- import Data.Code.TS
import Data.Function.AskName
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.HelloWorld
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
-- import Data.Person
import Data.Prims
import Data.Render.File.Longhand
import Data.Render.File.Imports
import Data.Render.File.Shorthand
import Data.Render.Statement.Longhand
import Data.Render.Statement.Shorthand
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
        createDirectoryIfMissing True "data/examples/statements/Longhand/haskell"

        BSL.writeFile "data/examples/statements/Longhand/haskell/askName.hs" $ renderStatementLonghand (askName :: HS () ())
        BSL.writeFile "data/examples/statements/Longhand/haskell/collatzStep.hs" $ renderStatementLonghand (collatzStep :: HS Int Int)
        BSL.writeFile "data/examples/statements/Longhand/haskell/helloWorld.hs" $ renderStatementLonghand (helloWorld :: HS () ())
        BSL.writeFile "data/examples/statements/Longhand/haskell/isPalindrome.hs" $ renderStatementLonghand (isPalindrome :: HS String Bool)
        BSL.writeFile "data/examples/statements/Longhand/haskell/greetTuple.hs" $ renderStatementLonghand (greetTuple :: HS (String, Int) String)
        BSL.writeFile "data/examples/statements/Longhand/haskell/reverseInput.hs" $ renderStatementLonghand (revInputProgram :: HS () ())

        createDirectoryIfMissing True "data/examples/statements/shorthand/haskell"

        BSL.writeFile "data/examples/statements/shorthand/haskell/askName.hs" $ renderStatementShorthand (askName :: HS () ())
        BSL.writeFile "data/examples/statements/shorthand/haskell/collatzStep.hs" $ renderStatementShorthand (collatzStep :: HS Int Int)
        BSL.writeFile "data/examples/statements/shorthand/haskell/helloWorld.hs" $ renderStatementShorthand (helloWorld :: HS () ())
        BSL.writeFile "data/examples/statements/shorthand/haskell/isPalindrome.hs" $ renderStatementShorthand (isPalindrome :: HS String Bool)
        BSL.writeFile "data/examples/statements/shorthand/haskell/greetTuple.hs" $ renderStatementShorthand (greetTuple :: HS (String, Int) String)
        BSL.writeFile "data/examples/statements/Longhand/haskell/reverseInput.hs" $ renderStatementShorthand (revInputProgram :: HS () ())

        createDirectoryIfMissing True "data/examples/programs/Imports/haskell"

        BSL.writeFile "data/examples/programs/Imports/haskell/askName.hs" $ renderFileImports (askName :: HS () ())
        BSL.writeFile "data/examples/programs/Imports/haskell/collatzStep.hs" $ renderFileImports (collatzStep :: HS Int Int)
        BSL.writeFile "data/examples/programs/Imports/haskell/helloWorld.hs" $ renderFileImports (helloWorld :: HS () ())
        BSL.writeFile "data/examples/programs/Imports/haskell/isPalindrome.hs" $ renderFileImports (isPalindrome :: HS String Bool)
        BSL.writeFile "data/examples/programs/Imports/haskell/greetTuple.hs" $ renderFileImports (greetTuple :: HS (String, Int) String)
        BSL.writeFile "data/examples/programs/Imports/haskell/reverseInput.hs" $ renderFileImports (revInputProgram :: HS () ())

        createDirectoryIfMissing True "data/examples/programs/Longhand/haskell"

        BSL.writeFile "data/examples/programs/Longhand/haskell/askName.hs" $ renderFileLonghand (askName :: HS () ())
        BSL.writeFile "data/examples/programs/Longhand/haskell/collatzStep.hs" $ renderFileLonghand (collatzStep :: HS Int Int)
        BSL.writeFile "data/examples/programs/Longhand/haskell/helloWorld.hs" $ renderFileLonghand (helloWorld :: HS () ())
        BSL.writeFile "data/examples/programs/Longhand/haskell/isPalindrome.hs" $ renderFileLonghand (isPalindrome :: HS String Bool)
        BSL.writeFile "data/examples/programs/Longhand/haskell/greetTuple.hs" $ renderFileLonghand (greetTuple :: HS (String, Int) String)
        BSL.writeFile "data/examples/programs/Longhand/haskell/reverseInput.hs" $ renderFileLonghand (revInputProgram :: HS () ())

        createDirectoryIfMissing True "data/examples/programs/Shorthand/haskell"

        BSL.writeFile "data/examples/programs/Shorthand/haskell/askName.hs" $ renderFileShorthand (askName :: HS () ())
        BSL.writeFile "data/examples/programs/Shorthand/haskell/collatzStep.hs" $ renderFileShorthand (collatzStep :: HS Int Int)
        BSL.writeFile "data/examples/programs/Shorthand/haskell/helloWorld.hs" $ renderFileShorthand (helloWorld :: HS () ())
        BSL.writeFile "data/examples/programs/Shorthand/haskell/isPalindrome.hs" $ renderFileShorthand (isPalindrome :: HS String Bool)
        BSL.writeFile "data/examples/programs/Shorthand/haskell/greetTuple.hs" $ renderFileShorthand (greetTuple :: HS (String, Int) String)
        BSL.writeFile "data/examples/programs/Shorthand/haskell/reverseInput.hs" $ renderFileShorthand (revInputProgram :: HS () ())

    {-
    do
        createDirectoryIfMissing True "data/examples/statements/Longhand/js"

        BSL.writeFile "data/examples/statements/Longhand/js/askName.js" $ renderStatementLonghand (askName :: JS () ())
        BSL.writeFile "data/examples/statements/Longhand/js/collatzStep.js" $ renderStatementLonghand (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/statements/Longhand/js/helloWorld.js" $ renderStatementLonghand (helloWorld :: JS () ())
        BSL.writeFile "data/examples/statements/Longhand/js/isPalindrome.js" $ renderStatementLonghand (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/statements/Longhand/js/greetTuple.js" $ renderStatementLonghand (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/statements/Longhand/js/reverseInput.js" $ renderStatementLonghand (revInputProgram :: JS () ())

        createDirectoryIfMissing True "data/examples/statements/shorthand/js"

        BSL.writeFile "data/examples/statements/shorthand/js/askName.js" $ renderStatementShorthand (askName :: JS () ())
        BSL.writeFile "data/examples/statements/shorthand/js/collatzStep.js" $ renderStatementShorthand (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/statements/shorthand/js/helloWorld.js" $ renderStatementShorthand (helloWorld :: JS () ())
        BSL.writeFile "data/examples/statements/shorthand/js/isPalindrome.js" $ renderStatementShorthand (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/statements/shorthand/js/greetTuple.js" $ renderStatementShorthand (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/statements/shorthand/js/reverseInput.js" $ renderStatementShorthand (revInputProgram :: JS () ())

        createDirectoryIfMissing True "data/examples/programs/Imports/js"

        BSL.writeFile "data/examples/programs/Imports/js/askName.js" $ renderFileImports (askName :: JS () ())
        BSL.writeFile "data/examples/programs/Imports/js/collatzStep.js" $ renderFileImports (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/programs/Imports/js/helloWorld.js" $ renderFileImports (helloWorld :: JS () ())
        BSL.writeFile "data/examples/programs/Imports/js/isPalindrome.js" $ renderFileImports (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/programs/Imports/js/greetTuple.js" $ renderFileImports (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/programs/Imports/js/reverseInput.js" $ renderFileImports (revInputProgram :: JS () ())

        createDirectoryIfMissing True "data/examples/programs/Longhand/js"

        BSL.writeFile "data/examples/programs/Longhand/js/askName.js" $ renderFileLonghand (askName :: JS () ())
        BSL.writeFile "data/examples/programs/Longhand/js/collatzStep.js" $ renderFileLonghand (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/programs/Longhand/js/helloWorld.js" $ renderFileLonghand (helloWorld :: JS () ())
        BSL.writeFile "data/examples/programs/Longhand/js/isPalindrome.js" $ renderFileLonghand (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/programs/Longhand/js/greetTuple.js" $ renderFileLonghand (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/programs/Longhand/js/reverseInput.js" $ renderFileLonghand (revInputProgram :: JS () ())

        createDirectoryIfMissing True "data/examples/programs/witjshorthand/js"

        BSL.writeFile "data/examples/programs/Shorthand/js/askName.js" $ renderFileShorthand (askName :: JS () ())
        BSL.writeFile "data/examples/programs/Shorthand/js/collatzStep.js" $ renderFileShorthand (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/programs/Shorthand/js/helloWorld.js" $ renderFileShorthand (helloWorld :: JS () ())
        BSL.writeFile "data/examples/programs/Shorthand/js/isPalindrome.js" $ renderFileShorthand (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/programs/Shorthand/js/greetTuple.js" $ renderFileShorthand (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/programs/Shorthand/js/reverseInput.js" $ renderFileShorthand (revInputProgram :: JS () ())

    
    do
        createDirectoryIfMissing True "data/examples/statements/Longhand/ts"

        BSL.writeFile "data/examples/statements/Longhand/ts/askName.ts" $ renderStatementLonghand (askName :: TS () ())
        BSL.writeFile "data/examples/statements/Longhand/ts/collatzStep.ts" $ renderStatementLonghand (collatzStep :: TS Int Int)
        BSL.writeFile "data/examples/statements/Longhand/ts/helloWorld.ts" $ renderStatementLonghand (helloWorld :: TS () ())
        BSL.writeFile "data/examples/statements/Longhand/ts/isPalindrome.ts" $ renderStatementLonghand (isPalindrome :: TS String Bool)
        BSL.writeFile "data/examples/statements/Longhand/ts/greetTuple.ts" $ renderStatementLonghand (greetTuple :: TS (String, Int) String)
        BSL.writeFile "data/examples/statements/Longhand/ts/reverseInput.ts" $ renderStatementLonghand (revInputProgram :: TS () ())

        createDirectoryIfMissing True "data/examples/statements/shorthand/ts"

        BSL.writeFile "data/examples/statements/shorthand/ts/askName.ts" $ renderStatementShorthand (askName :: TS () ())
        BSL.writeFile "data/examples/statements/shorthand/ts/collatzStep.ts" $ renderStatementShorthand (collatzStep :: TS Int Int)
        BSL.writeFile "data/examples/statements/shorthand/ts/helloWorld.ts" $ renderStatementShorthand (helloWorld :: TS () ())
        BSL.writeFile "data/examples/statements/shorthand/ts/isPalindrome.ts" $ renderStatementShorthand (isPalindrome :: TS String Bool)
        BSL.writeFile "data/examples/statements/shorthand/ts/greetTuple.ts" $ renderStatementShorthand (greetTuple :: TS (String, Int) String)
        BSL.writeFile "data/examples/statements/shorthand/ts/reverseInput.ts" $ renderStatementShorthand (revInputProgram :: TS () ())

        createDirectoryIfMissing True "data/examples/programs/Imports/ts"

        BSL.writeFile "data/examples/programs/Imports/ts/askName.ts" $ renderFileImports (askName :: TS () ())
        BSL.writeFile "data/examples/programs/Imports/ts/collatzStep.ts" $ renderFileImports (collatzStep :: TS Int Int)
        BSL.writeFile "data/examples/programs/Imports/ts/helloWorld.ts" $ renderFileImports (helloWorld :: TS () ())
        BSL.writeFile "data/examples/programs/Imports/ts/isPalindrome.ts" $ renderFileImports (isPalindrome :: TS String Bool)
        BSL.writeFile "data/examples/programs/Imports/ts/greetTuple.ts" $ renderFileImports (greetTuple :: TS (String, Int) String)
        BSL.writeFile "data/examples/programs/Imports/ts/reverseInput.ts" $ renderFileImports (revInputProgram :: TS () ())

        createDirectoryIfMissing True "data/examples/programs/Longhand/ts"

        BSL.writeFile "data/examples/programs/Longhand/ts/askName.ts" $ renderFileLonghand (askName :: TS () ())
        BSL.writeFile "data/examples/programs/Longhand/ts/collatzStep.ts" $ renderFileLonghand (collatzStep :: TS Int Int)
        BSL.writeFile "data/examples/programs/Longhand/ts/helloWorld.ts" $ renderFileLonghand (helloWorld :: TS () ())
        BSL.writeFile "data/examples/programs/Longhand/ts/isPalindrome.ts" $ renderFileLonghand (isPalindrome :: TS String Bool)
        BSL.writeFile "data/examples/programs/Longhand/ts/greetTuple.ts" $ renderFileLonghand (greetTuple :: TS (String, Int) String)
        BSL.writeFile "data/examples/programs/Longhand/ts/reverseInput.ts" $ renderFileLonghand (revInputProgram :: TS () ())

        createDirectoryIfMissing True "data/examples/programs/witjshorthand/ts"

        BSL.writeFile "data/examples/programs/Shorthand/ts/askName.ts" $ renderFileShorthand (askName :: TS () ())
        BSL.writeFile "data/examples/programs/Shorthand/ts/collatzStep.ts" $ renderFileShorthand (collatzStep :: TS Int Int)
        BSL.writeFile "data/examples/programs/Shorthand/ts/helloWorld.ts" $ renderFileShorthand (helloWorld :: TS () ())
        BSL.writeFile "data/examples/programs/Shorthand/ts/isPalindrome.ts" $ renderFileShorthand (isPalindrome :: TS String Bool)
        BSL.writeFile "data/examples/programs/Shorthand/ts/greetTuple.ts" $ renderFileShorthand (greetTuple :: TS (String, Int) String)
        BSL.writeFile "data/examples/programs/Shorthand/ts/reverseInput.ts" $ renderFileShorthand (revInputProgram :: TS () ())

    do
        createDirectoryIfMissing True "data/examples/statements/Longhand/php"

        BSL.writeFile "data/examples/statements/Longhand/php/askName.php" $ renderStatementLonghand (askName :: PHP () ())
        BSL.writeFile "data/examples/statements/Longhand/php/collatzStep.php" $ renderStatementLonghand (collatzStep :: PHP Int Int)
        BSL.writeFile "data/examples/statements/Longhand/php/helloWorld.php" $ renderStatementLonghand (helloWorld :: PHP () ())
        BSL.writeFile "data/examples/statements/Longhand/php/isPalindrome.php" $ renderStatementLonghand (isPalindrome :: PHP String Bool)
        BSL.writeFile "data/examples/statements/Longhand/php/greetTuple.php" $ renderStatementLonghand (greetTuple :: PHP (String, Int) String)
        BSL.writeFile "data/examples/statements/Longhand/php/reverseInput.php" $ renderStatementLonghand (revInputProgram :: PHP () ())

        createDirectoryIfMissing True "data/examples/statements/shorthand/php"

        BSL.writeFile "data/examples/statements/shorthand/php/askName.php" $ renderStatementShorthand (askName :: PHP () ())
        BSL.writeFile "data/examples/statements/shorthand/php/collatzStep.php" $ renderStatementShorthand (collatzStep :: PHP Int Int)
        BSL.writeFile "data/examples/statements/shorthand/php/helloWorld.php" $ renderStatementShorthand (helloWorld :: PHP () ())
        BSL.writeFile "data/examples/statements/shorthand/php/isPalindrome.php" $ renderStatementShorthand (isPalindrome :: PHP String Bool)
        BSL.writeFile "data/examples/statements/shorthand/php/greetTuple.php" $ renderStatementShorthand (greetTuple :: PHP (String, Int) String)
        BSL.writeFile "data/examples/statements/shorthand/php/reverseInput.php" $ renderStatementShorthand (revInputProgram :: PHP () ())

        createDirectoryIfMissing True "data/examples/programs/Imports/php"

        BSL.writeFile "data/examples/programs/Imports/php/askName.php" $ renderFileImports (askName :: PHP () ())
        BSL.writeFile "data/examples/programs/Imports/php/collatzStep.php" $ renderFileImports (collatzStep :: PHP Int Int)
        BSL.writeFile "data/examples/programs/Imports/php/helloWorld.php" $ renderFileImports (helloWorld :: PHP () ())
        BSL.writeFile "data/examples/programs/Imports/php/isPalindrome.php" $ renderFileImports (isPalindrome :: PHP String Bool)
        BSL.writeFile "data/examples/programs/Imports/php/greetTuple.php" $ renderFileImports (greetTuple :: PHP (String, Int) String)
        BSL.writeFile "data/examples/programs/Imports/php/reverseInput.php" $ renderFileImports (revInputProgram :: PHP () ())

        createDirectoryIfMissing True "data/examples/programs/Longhand/php"

        BSL.writeFile "data/examples/programs/Longhand/php/askName.php" $ renderFileLonghand (askName :: PHP () ())
        BSL.writeFile "data/examples/programs/Longhand/php/collatzStep.php" $ renderFileLonghand (collatzStep :: PHP Int Int)
        BSL.writeFile "data/examples/programs/Longhand/php/helloWorld.php" $ renderFileLonghand (helloWorld :: PHP () ())
        BSL.writeFile "data/examples/programs/Longhand/php/isPalindrome.php" $ renderFileLonghand (isPalindrome :: PHP String Bool)
        BSL.writeFile "data/examples/programs/Longhand/php/greetTuple.php" $ renderFileLonghand (greetTuple :: PHP (String, Int) String)
        BSL.writeFile "data/examples/programs/Longhand/php/reverseInput.php" $ renderFileLonghand (revInputProgram :: PHP () ())

        createDirectoryIfMissing True "data/examples/programs/witjshorthand/php"

        BSL.writeFile "data/examples/programs/Shorthand/php/askName.php" $ renderFileShorthand (askName :: PHP () ())
        BSL.writeFile "data/examples/programs/Shorthand/php/collatzStep.php" $ renderFileShorthand (collatzStep :: PHP Int Int)
        BSL.writeFile "data/examples/programs/Shorthand/php/helloWorld.php" $ renderFileShorthand (helloWorld :: PHP () ())
        BSL.writeFile "data/examples/programs/Shorthand/php/isPalindrome.php" $ renderFileShorthand (isPalindrome :: PHP String Bool)
        BSL.writeFile "data/examples/programs/Shorthand/php/greetTuple.php" $ renderFileShorthand (greetTuple :: PHP (String, Int) String)
        BSL.writeFile "data/examples/programs/Shorthand/php/reverseInput.php" $ renderFileShorthand (revInputProgram :: PHP () ())
    -}