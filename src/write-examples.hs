{-# LANGUAGE Unsafe #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unsafe -Wwarn #-}

module Main (main) where

-- import Control.Category.Interpret
import Data.Aeson                            qualified as A
-- import Data.ByteString.Char8      qualified as BS
import Data.ByteString.Lazy.Char8            qualified as BSL
import Data.Code.Haskell
import Data.Code.JS
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
        createDirectoryIfMissing True "data/examples/statements/longhand/haskell"

        BSL.writeFile "data/examples/statements/longhand/haskell/askName.hs" $ renderStatementLonghand (askName :: HS () ())
        BSL.writeFile "data/examples/statements/longhand/haskell/collatzStep.hs" $ renderStatementLonghand (collatzStep :: HS Int Int)
        BSL.writeFile "data/examples/statements/longhand/haskell/helloWorld.hs" $ renderStatementLonghand (helloWorld :: HS () ())
        BSL.writeFile "data/examples/statements/longhand/haskell/isPalindrome.hs" $ renderStatementLonghand (isPalindrome :: HS String Bool)
        BSL.writeFile "data/examples/statements/longhand/haskell/greetTuple.hs" $ renderStatementLonghand (greetTuple :: HS (String, Int) String)
        BSL.writeFile "data/examples/statements/longhand/haskell/reverseInput.hs" $ renderStatementLonghand (revInputProgram :: HS () ())

        createDirectoryIfMissing True "data/examples/statements/shorthand/haskell"

        BSL.writeFile "data/examples/statements/shorthand/haskell/askName.hs" $ renderStatementShorthand (askName :: HS () ())
        BSL.writeFile "data/examples/statements/shorthand/haskell/collatzStep.hs" $ renderStatementShorthand (collatzStep :: HS Int Int)
        BSL.writeFile "data/examples/statements/shorthand/haskell/helloWorld.hs" $ renderStatementShorthand (helloWorld :: HS () ())
        BSL.writeFile "data/examples/statements/shorthand/haskell/isPalindrome.hs" $ renderStatementShorthand (isPalindrome :: HS String Bool)
        BSL.writeFile "data/examples/statements/shorthand/haskell/greetTuple.hs" $ renderStatementShorthand (greetTuple :: HS (String, Int) String)
        BSL.writeFile "data/examples/statements/longhand/haskell/reverseInput.hs" $ renderStatementShorthand (revInputProgram :: HS () ())

        createDirectoryIfMissing True "data/examples/programs/imports/haskell"

        BSL.writeFile "data/examples/programs/imports/haskell/askName.hs" $ renderFileImports "Main" "main" "()" "IO ()" (askName :: HS () ())
        BSL.writeFile "data/examples/programs/imports/haskell/CollatzStep.hs" $ renderFileImports "CollatzStep" "collatzStep" "Int" "Int" (collatzStep :: HS Int Int)
        BSL.writeFile "data/examples/programs/imports/haskell/HelloWorld.hs" $ renderFileImports "Main" "main" "()" "IO ()" (helloWorld :: HS () ())
        BSL.writeFile "data/examples/programs/imports/haskell/IsPalindrome.hs" $ renderFileImports "IsPalindrome" "isPalindrome" "String" "Bool" (isPalindrome :: HS String Bool)
        BSL.writeFile "data/examples/programs/imports/haskell/GreetTuple.hs" $ renderFileImports "GreetTuple" "greetTuple" "(String, Int)" "String" (greetTuple :: HS (String, Int) String)
        BSL.writeFile "data/examples/programs/imports/haskell/reverseInput.hs" $ renderFileImports "Main" "main" "()" "IO ()" (revInputProgram :: HS () ())

        createDirectoryIfMissing True "data/examples/programs/longhand/haskell"

        BSL.writeFile "data/examples/programs/longhand/haskell/askName.hs" $ renderFileLonghand "Main" "main" "()" "IO ()" (askName :: HS () ())
        BSL.writeFile "data/examples/programs/longhand/haskell/CollatzStep.hs" $ renderFileLonghand "CollatzStep" "collatzStep" "Int" "Int" (collatzStep :: HS Int Int)
        BSL.writeFile "data/examples/programs/longhand/haskell/helloWorld.hs" $ renderFileLonghand "Main" "main" "()" "IO ()" (helloWorld :: HS () ())
        BSL.writeFile "data/examples/programs/longhand/haskell/IsPalindrome.hs" $ renderFileLonghand "IsPalindrome" "isPalindrome" "String" "Bool" (isPalindrome :: HS String Bool)
        BSL.writeFile "data/examples/programs/longhand/haskell/GreetTuple.hs" $ renderFileLonghand "GreetTuple" "greetTuple" "(String, Int)" "String" (greetTuple :: HS (String, Int) String)
        BSL.writeFile "data/examples/programs/longhand/haskell/reverseInput.hs" $ renderFileLonghand "Main" "main" "()" "IO ()" (revInputProgram :: HS () ())

        createDirectoryIfMissing True "data/examples/programs/shorthand/haskell"

        BSL.writeFile "data/examples/programs/shorthand/haskell/askName.hs" $ renderFileShorthand "Main" "main" "()" "IO ()" (askName :: HS () ())
        BSL.writeFile "data/examples/programs/shorthand/haskell/CollatzStep.hs" $ renderFileShorthand "CollatzStep" "collatzStep" "Int" "Int" (collatzStep :: HS Int Int)
        BSL.writeFile "data/examples/programs/shorthand/haskell/helloWorld.hs" $ renderFileShorthand "Main" "main" "()" "IO ()" (helloWorld :: HS () ())
        BSL.writeFile "data/examples/programs/shorthand/haskell/IsPalindrome.hs" $ renderFileShorthand "IsPalindrome" "isPalindrome" "String" "Bool" (isPalindrome :: HS String Bool)
        BSL.writeFile "data/examples/programs/shorthand/haskell/GreetTuple.hs" $ renderFileShorthand "GreetTuple" "greetTuple" "(String, Int)" "String" (greetTuple :: HS (String, Int) String)
        BSL.writeFile "data/examples/programs/shorthand/haskell/reverseInput.hs" $ renderFileShorthand "Main" "main" "()" "IO ()" (revInputProgram :: HS () ())
    do
        createDirectoryIfMissing True "data/examples/statements/longhand/js"

        BSL.writeFile "data/examples/statements/longhand/js/askName.js" $ renderStatementLonghand (askName :: JS () ())
        BSL.writeFile "data/examples/statements/longhand/js/collatzStep.js" $ renderStatementLonghand (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/statements/longhand/js/helloWorld.js" $ renderStatementLonghand (helloWorld :: JS () ())
        BSL.writeFile "data/examples/statements/longhand/js/isPalindrome.js" $ renderStatementLonghand (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/statements/longhand/js/greetTuple.js" $ renderStatementLonghand (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/statements/longhand/js/reverseInput.js" $ renderStatementLonghand (revInputProgram :: JS () ())

        createDirectoryIfMissing True "data/examples/statements/shorthand/js"

        BSL.writeFile "data/examples/statements/shorthand/js/askName.js" $ renderStatementShorthand (askName :: JS () ())
        BSL.writeFile "data/examples/statements/shorthand/js/collatzStep.js" $ renderStatementShorthand (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/statements/shorthand/js/helloWorld.js" $ renderStatementShorthand (helloWorld :: JS () ())
        BSL.writeFile "data/examples/statements/shorthand/js/isPalindrome.js" $ renderStatementShorthand (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/statements/shorthand/js/greetTuple.js" $ renderStatementShorthand (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/statements/longhand/js/reverseInput.js" $ renderStatementShorthand (revInputProgram :: JS () ())

        createDirectoryIfMissing True "data/examples/programs/imports/js"

        BSL.writeFile "data/examples/programs/imports/js/askName.js" $ renderFileImports "main" "main" "void" "void" (askName :: JS () ())
        BSL.writeFile "data/examples/programs/imports/js/collatzStep.js" $ renderFileImports "collatzStep" "collatzStep" "number" "number" (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/programs/imports/js/helloWorld.js" $ renderFileImports "main" "main" "void" "void" (helloWorld :: JS () ())
        BSL.writeFile "data/examples/programs/imports/js/isPalindrome.js" $ renderFileImports "isPalindrome" "isPalindrome" "string" "boolean" (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/programs/imports/js/greetTuple.js" $ renderFileImports "greetTuple" "greetTuple" "[string, number]" "string" (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/programs/imports/js/reverseInput.js" $ renderFileImports "main" "main" "void" "void" (revInputProgram :: JS () ())

        createDirectoryIfMissing True "data/examples/programs/longhand/js"

        BSL.writeFile "data/examples/programs/longhand/js/askName.js" $ renderFileLonghand "main" "main" "void" "void" (askName :: JS () ())
        BSL.writeFile "data/examples/programs/longhand/js/collatzStep.js" $ renderFileLonghand "collatzStep" "collatzStep" "number" "number" (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/programs/longhand/js/helloWorld.js" $ renderFileLonghand "main" "main" "void" "void" (helloWorld :: JS () ())
        BSL.writeFile "data/examples/programs/longhand/js/isPalindrome.js" $ renderFileLonghand "isPalindrome" "isPalindrome" "string" "boolean" (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/programs/longhand/js/greetTuple.js" $ renderFileLonghand "greetTuple" "greetTuple" "[string, number]" "string" (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/programs/longhand/js/reverseInput.js" $ renderFileLonghand "main" "main" "void" "void" (revInputProgram :: JS () ())

        createDirectoryIfMissing True "data/examples/programs/shorthand/js"

        BSL.writeFile "data/examples/programs/shorthand/js/askName.js" $ renderFileShorthand "main" "main" "void" "void" (askName :: JS () ())
        BSL.writeFile "data/examples/programs/shorthand/js/collatzStep.js" $ renderFileShorthand "collatzStep" "collatzStep" "number" "number" (collatzStep :: JS Int Int)
        BSL.writeFile "data/examples/programs/shorthand/js/helloWorld.js" $ renderFileShorthand "main" "main" "void" "void" (helloWorld :: JS () ())
        BSL.writeFile "data/examples/programs/shorthand/js/isPalindrome.js" $ renderFileShorthand "isPalindrome" "isPalindrome" "string" "boolean" (isPalindrome :: JS String Bool)
        BSL.writeFile "data/examples/programs/shorthand/js/greetTuple.js" $ renderFileShorthand "greetTuple" "greetTuple" "[string, number]" "string" (greetTuple :: JS (String, Int) String)
        BSL.writeFile "data/examples/programs/shorthand/js/reverseInput.js" $ renderFileShorthand "main" "main" "void" "void" (revInputProgram :: JS () ())
