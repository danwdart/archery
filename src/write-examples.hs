{-# LANGUAGE Unsafe #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unsafe -Wwarn #-}

module Main (main) where

-- import Control.Category.Compile.Imports
-- import Control.Category.Compile.Longhand
-- import Control.Category.Compile.Shorthand
-- import Control.Category.Interpret
import Data.Aeson                            qualified as A
import Data.Bifunctor
-- import Data.ByteString.Char8      qualified as BS
import Data.ByteString.Lazy.Char8            qualified as BSL
import Data.Code.Haskell
import Data.Code.JS
-- import Data.Code.PHP
-- import Data.Code.TS
import Data.Foldable
import Data.Function.AskName
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.HelloWorld
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
-- import Data.Person
import Data.Prims

-- import Data.Render.Library.Longhand
-- import Data.Render.Library.Imports
-- import Data.Render.Library.Shorthand
import Data.Render.Program.Longhand
import Data.Render.Program.Imports
import Data.Render.Program.Shorthand
import Data.Render.Statement.Longhand
import Data.Render.Statement.Shorthand
-- import Data.Person
import Data.Yaml                             qualified as Y
import System.Directory
import System.FilePath
import Data.Render.Library.Imports (RenderLibraryImports(renderLibraryImports))

writeToPrefix :: FilePath -> [(FilePath, BSL.ByteString)] -> IO ()
writeToPrefix prefix = traverse_ ((\(file, contents) -> createDirectoryIfMissing True (dropFileName file) >> BSL.writeFile file contents) . first (prefix <>))


main ∷ IO ()
main = do
    removeDirectoryRecursive "data/examples"

    -- removeDirectoryRecursive "data/examples/jcat"

    do -- jcat
        do -- lib
            createDirectoryIfMissing True "data/examples/jcat/lib"

            A.encodeFile "data/examples/jcat/lib/collatzStep.json" (collatzStep :: FreeFunc Prims Int Int)
            A.encodeFile "data/examples/jcat/lib/isPalindrome.json" (isPalindrome :: FreeFunc Prims String Bool)
            -- A.encodeFile "data/examples/jcat/lib/greetData.json" (greetData :: FreeFunc Prims Person String)
            A.encodeFile "data/examples/jcat/lib/greetTuple.json" (greetTuple :: FreeFunc Prims (String, Int) String)
        do -- src
            createDirectoryIfMissing True "data/examples/jcat/src"

            A.encodeFile "data/examples/jcat/srcaskName.json" (askName :: FreeFunc Prims () ())
            A.encodeFile "data/examples/jcat/srchelloWorld.json" (helloWorld :: FreeFunc Prims () ())
            A.encodeFile "data/examples/jcat/srcreverseInput.json" (revInputProgram :: FreeFunc Prims () ())
    do -- ycat
        do -- lib
            createDirectoryIfMissing True "data/examples/ycat/lib"

            Y.encodeFile "data/examples/ycat/lib/collatzStep.yaml" (collatzStep :: FreeFunc Prims Int Int)
            Y.encodeFile "data/examples/ycat/lib/isPalindrome.yaml" (isPalindrome :: FreeFunc Prims String Bool)
            -- Y.encodeFile "data/examples/ycat/lib/greetData.yaml" (greetData :: FreeFunc Prims Person String)
            Y.encodeFile "data/examples/ycat/lib/greetTuple.yaml" (greetTuple :: FreeFunc Prims (String, Int) String)
        do -- src
            createDirectoryIfMissing True "data/examples/ycat/src"

            Y.encodeFile "data/examples/ycat/src/askName.yaml" (askName :: FreeFunc Prims () ())
            Y.encodeFile "data/examples/ycat/src/helloWorld.yaml" (helloWorld :: FreeFunc Prims () ())
            Y.encodeFile "data/examples/ycat/src/reverseInput.yaml" (revInputProgram :: FreeFunc Prims () ())
    do -- HS
        do -- St
            do -- LH
                do -- lib
                    createDirectoryIfMissing True "data/examples/statements/longhand/haskell/lib"
                    
                    writeToPrefix "data/examples/libraries/imports/haskell/collatzStep/" $ renderLibraryImports (collatzStep :: HS Int Int)

                    -- BSL.writeFile "data/examples/statements/longhand/haskell/lib/CollatzStep.hs" $ renderStatementLonghand (collatzStep :: HS Int Int)
                    -- BSL.writeFile "data/examples/statements/longhand/haskell/lib/IsPalindrome.hs" $ renderStatementLonghand (isPalindrome :: HS String Bool)
                    -- BSL.writeFile "data/examples/statements/longhand/haskell/lib/GreetTuple.hs" $ renderStatementLonghand (greetTuple :: HS (String, Int) String)
                do -- src
                    createDirectoryIfMissing True "data/examples/statements/longhand/haskell/src"

                    BSL.writeFile "data/examples/statements/longhand/haskell/src/askName.hs" $ renderStatementLonghand (askName :: HS () ())
                    BSL.writeFile "data/examples/statements/longhand/haskell/src/helloWorld.hs" $ renderStatementLonghand (helloWorld :: HS () ())
                    BSL.writeFile "data/examples/statements/longhand/haskell/src/reverseInput.hs" $ renderStatementLonghand (revInputProgram :: HS () ())
            do -- SH
                do -- lib
                    createDirectoryIfMissing True "data/examples/statements/shorthand/haskell/lib"

                    -- BSL.writeFile "data/examples/statements/shorthand/haskell/lib/collatzStep.hs" $ renderStatementShorthand (collatzStep :: HS Int Int)
                    -- BSL.writeFile "data/examples/statements/shorthand/haskell/lib/isPalindrome.hs" $ renderStatementShorthand (isPalindrome :: HS String Bool)
                    -- BSL.writeFile "data/examples/statements/shorthand/haskell/lib/greetTuple.hs" $ renderStatementShorthand (greetTuple :: HS (String, Int) String)
                do -- src
                    createDirectoryIfMissing True "data/examples/statements/shorthand/haskell/src"

                    BSL.writeFile "data/examples/statements/shorthand/haskell/src/askName.hs" $ renderStatementShorthand (askName :: HS () ())
                    BSL.writeFile "data/examples/statements/shorthand/haskell/src/helloWorld.hs" $ renderStatementShorthand (helloWorld :: HS () ())
                    BSL.writeFile "data/examples/statements/shorthand/haskell/src/reverseInput.hs" $ renderStatementShorthand (revInputProgram :: HS () ())
        do -- Pr
            do -- Im
                do -- lib
                    createDirectoryIfMissing True "data/examples/libraries/imports/haskell"

                    -- BSL.writeFile "data/examples/libraries/imports/haskell/CollatzStep.hs" $ renderLibraryImports (collatzStep :: HS Int Int)
                    -- BSL.writeFile "data/examples/libraries/imports/haskell/IsPalindrome.hs" $ renderLibraryImports (isPalindrome :: HS String Bool)
                    -- BSL.writeFile "data/examples/libraries/imports/haskell/GreetTuple.hs" $ renderLibraryImports (greetTuple :: HS (String, Int) String)
                do -- src
                    createDirectoryIfMissing True "data/examples/programs/imports/haskell"

                    BSL.writeFile "data/examples/programs/imports/haskell/askName.hs" $ renderProgramImports (askName :: HS () ())
                    BSL.writeFile "data/examples/programs/imports/haskell/helloWorld.hs" $ renderProgramImports (helloWorld :: HS () ())
                    BSL.writeFile "data/examples/programs/imports/haskell/reverseInput.hs" $ renderProgramImports (revInputProgram :: HS () ())
            do -- LH
                do -- lib
                    createDirectoryIfMissing True "data/examples/libraries/longhand/haskell"

                    -- BSL.writeFile "data/examples/libraries/longhand/haskell/CollatzStep.hs" $ renderLibraryLonghand (collatzStep :: HS Int Int)
                    -- BSL.writeFile "data/examples/libraries/longhand/haskell/IsPalindrome.hs" $ renderLibraryLonghand (isPalindrome :: HS String Bool)
                    -- BSL.writeFile "data/examples/libraries/longhand/haskell/GreetTuple.hs" $ renderLibraryLonghand (greetTuple :: HS (String, Int) String)
                do -- src
                    createDirectoryIfMissing True "data/examples/programs/longhand/haskell"

                    BSL.writeFile "data/examples/programs/longhand/haskell/askName.hs" $ renderProgramLonghand (askName :: HS () ())
                    BSL.writeFile "data/examples/programs/longhand/haskell/helloWorld.hs" $ renderProgramLonghand (helloWorld :: HS () ())
                    BSL.writeFile "data/examples/programs/longhand/haskell/reverseInput.hs" $ renderProgramLonghand (revInputProgram :: HS () ())
            do -- SH
                do -- lib
                    createDirectoryIfMissing True "data/examples/libraries/shorthand/haskell"

                    -- BSL.writeFile "data/examples/libraries/shorthand/haskell/CollatzStep.hs" $ renderLibraryShorthand (collatzStep :: HS Int Int)
                    -- BSL.writeFile "data/examples/libraries/shorthand/haskell/IsPalindrome.hs" $ renderLibraryShorthand (isPalindrome :: HS String Bool)
                    -- BSL.writeFile "data/examples/libraries/shorthand/haskell/GreetTuple.hs" $ renderLibraryShorthand (greetTuple :: HS (String, Int) String)
                do -- src
                    createDirectoryIfMissing True "data/examples/programs/shorthand/haskell"

                    BSL.writeFile "data/examples/programs/shorthand/haskell/askName.hs" $ renderProgramShorthand (askName :: HS () ())
                    BSL.writeFile "data/examples/programs/shorthand/haskell/helloWorld.hs" $ renderProgramShorthand (helloWorld :: HS () ())
                    BSL.writeFile "data/examples/programs/shorthand/haskell/reverseInput.hs" $ renderProgramShorthand (revInputProgram :: HS () ())
    do -- JS
        do -- St
            do -- LH
                do -- lib
                    createDirectoryIfMissing True "data/examples/statements/longhand/js/lib"

                    -- BSL.writeFile "data/examples/statements/longhand/js/lib/CollatzStep.js" $ renderStatementLonghand (collatzStep :: JS Int Int)
                    -- BSL.writeFile "data/examples/statements/longhand/js/lib/IsPalindrome.js" $ renderStatementLonghand (isPalindrome :: JS String Bool)
                    -- BSL.writeFile "data/examples/statements/longhand/js/lib/GreetTuple.js" $ renderStatementLonghand (greetTuple :: JS (String, Int) String)
                do -- src
                    createDirectoryIfMissing True "data/examples/statements/longhand/js/src"

                    BSL.writeFile "data/examples/statements/longhand/js/src/askName.js" $ renderStatementLonghand (askName :: JS () ())
                    BSL.writeFile "data/examples/statements/longhand/js/src/helloWorld.js" $ renderStatementLonghand (helloWorld :: JS () ())
                    BSL.writeFile "data/examples/statements/longhand/js/src/reverseInput.js" $ renderStatementLonghand (revInputProgram :: JS () ())
            do -- SH
                do -- lib
                    createDirectoryIfMissing True "data/examples/statements/shorthand/js/lib"

                    -- BSL.writeFile "data/examples/statements/shorthand/js/lib/collatzStep.js" $ renderStatementShorthand (collatzStep :: JS Int Int)
                    -- BSL.writeFile "data/examples/statements/shorthand/js/lib/isPalindrome.js" $ renderStatementShorthand (isPalindrome :: JS String Bool)
                    -- BSL.writeFile "data/examples/statements/shorthand/js/lib/greetTuple.js" $ renderStatementShorthand (greetTuple :: JS (String, Int) String)
                do -- src
                    createDirectoryIfMissing True "data/examples/statements/shorthand/js/src"

                    BSL.writeFile "data/examples/statements/shorthand/js/src/askName.js" $ renderStatementShorthand (askName :: JS () ())
                    BSL.writeFile "data/examples/statements/shorthand/js/src/helloWorld.js" $ renderStatementShorthand (helloWorld :: JS () ())
                    BSL.writeFile "data/examples/statements/shorthand/js/src/reverseInput.js" $ renderStatementShorthand (revInputProgram :: JS () ())
        do -- Pr
            do -- Im
                do -- lib
                    createDirectoryIfMissing True "data/examples/libraries/imports/js"
                    -- traverse (\(fileName, fileContent) -> BSL.writeFile fileName fileContent) . first ("data/examples/libraries/imports/js/collatzStep/" <>) $ renderLibraryImports (collatzStep :: JS Int Int)
                    -- BSL.writeFile "data/examples/libraries/imports/js/CollatzStep.js" $ renderLibraryImports (collatzStep :: JS Int Int)
                    -- BSL.writeFile "data/examples/libraries/imports/js/IsPalindrome.js" $ renderLibraryImports (isPalindrome :: JS String Bool)
                    -- BSL.writeFile "data/examples/libraries/imports/js/GreetTuple.js" $ renderLibraryImports (greetTuple :: JS (String, Int) String)
                do -- src
                    createDirectoryIfMissing True "data/examples/programs/imports/js"

                    BSL.writeFile "data/examples/programs/imports/js/askName.js" $ renderProgramImports (askName :: JS () ())
                    BSL.writeFile "data/examples/programs/imports/js/helloWorld.js" $ renderProgramImports (helloWorld :: JS () ())
                    BSL.writeFile "data/examples/programs/imports/js/reverseInput.js" $ renderProgramImports (revInputProgram :: JS () ())
            do -- LH
                do -- lib
                    createDirectoryIfMissing True "data/examples/libraries/longhand/js"

                    -- BSL.writeFile "data/examples/libraries/longhand/js/CollatzStep.js" $ renderLibraryLonghand (collatzStep :: JS Int Int)
                    -- BSL.writeFile "data/examples/libraries/longhand/js/IsPalindrome.js" $ renderLibraryLonghand (isPalindrome :: JS String Bool)
                    -- BSL.writeFile "data/examples/libraries/longhand/js/GreetTuple.js" $ renderLibraryLonghand (greetTuple :: JS (String, Int) String)
                do -- src
                    createDirectoryIfMissing True "data/examples/programs/longhand/js"

                    BSL.writeFile "data/examples/programs/longhand/js/askName.js" $ renderProgramLonghand (askName :: JS () ())
                    BSL.writeFile "data/examples/programs/longhand/js/helloWorld.js" $ renderProgramLonghand (helloWorld :: JS () ())
                    BSL.writeFile "data/examples/programs/longhand/js/reverseInput.js" $ renderProgramLonghand (revInputProgram :: JS () ())
            do -- SH
                do -- lib
                    createDirectoryIfMissing True "data/examples/libraries/shorthand/js"

                    -- BSL.writeFile "data/examples/libraries/shorthand/js/CollatzStep.js" $ renderLibraryShorthand (collatzStep :: JS Int Int)
                    -- BSL.writeFile "data/examples/libraries/shorthand/js/IsPalindrome.js" $ renderLibraryShorthand (isPalindrome :: JS String Bool)
                    -- BSL.writeFile "data/examples/libraries/shorthand/js/GreetTuple.js" $ renderLibraryShorthand (greetTuple :: JS (String, Int) String)
                do -- src
                    createDirectoryIfMissing True "data/examples/programs/shorthand/js"

                    BSL.writeFile "data/examples/programs/shorthand/js/askName.js" $ renderProgramShorthand (askName :: JS () ())
                    BSL.writeFile "data/examples/programs/shorthand/js/helloWorld.js" $ renderProgramShorthand (helloWorld :: JS () ())
                    BSL.writeFile "data/examples/programs/shorthand/js/reverseInput.js" $ renderProgramShorthand (revInputProgram :: JS () ())
    -- etc