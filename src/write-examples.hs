{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main (main) where

-- import Control.Category.Interpret
import Data.Aeson                  qualified as A
-- import Data.ByteString.Char8      qualified as BS
import Data.ByteString.Lazy.Char8  qualified as BSL
import Data.Code.Haskell.Func
import Data.Code.Haskell.Lamb
import Data.Code.Haskell.Mock
import Data.Code.JS.Lamb
import Data.Code.JS.Func
import Data.Code.PHP.Lamb
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
-- import Data.Person
import Data.Prims
import Data.Render.File
import Data.Render.Statement
-- import Data.Person
import Data.Yaml                   qualified as Y
import System.Directory

main ∷ IO ()
main = do
    removeDirectoryRecursive "data/examples"

    -- removeDirectoryRecursive "data/examples/jcat"

    createDirectoryIfMissing True "data/examples/jcat"

    A.encodeFile "data/examples/jcat/collatzStep.json" (collatzStep :: FreeFunc Prims Int Int)
    A.encodeFile "data/examples/jcat/isPalindrome.json" (isPalindrome :: FreeFunc Prims String Bool)
    -- A.encodeFile "data/examples/jcat/greetData.json" (greetData :: FreeFunc Prims Person String)
    A.encodeFile "data/examples/jcat/greetTuple.json" (greetTuple :: FreeFunc Prims (String, Int) String)
    A.encodeFile "data/examples/jcat/reverseInput.json" (revInputProgram :: FreeFunc Prims () ())

    createDirectoryIfMissing True "data/examples/ycat"

    Y.encodeFile "data/examples/ycat/collatzStep.yaml" (collatzStep :: FreeFunc Prims Int Int)
    Y.encodeFile "data/examples/ycat/isPalindrome.yaml" (isPalindrome :: FreeFunc Prims String Bool)
    -- Y.encodeFile "data/examples/ycat/greetData.yaml" (greetData :: FreeFunc Prims Person String)
    Y.encodeFile "data/examples/ycat/greetTuple.yaml" (greetTuple :: FreeFunc Prims (String, Int) String)
    Y.encodeFile "data/examples/ycat/reverseInput.yaml" (revInputProgram :: FreeFunc Prims () ())

    -- removeDirectoryRecursive "data/examples/statements"

    createDirectoryIfMissing True "data/examples/statements/haskell/hsfunc"

    BSL.writeFile "data/examples/statements/haskell/hsfunc/collatzStep.hss" $ renderStatement (collatzStep :: HSFunc Int Int)
    BSL.writeFile "data/examples/statements/haskell/hsfunc/isPalindrome.hss" $ renderStatement (isPalindrome :: HSFunc String Bool)
    BSL.writeFile "data/examples/statements/haskell/hsfunc/greetTuple.hss" $ renderStatement (greetTuple :: HSFunc (String, Int) String)
    BSL.writeFile "data/examples/statements/haskell/hsfunc/reverseInput.hss" $ renderStatement (revInputProgram :: HSFunc () ())

    createDirectoryIfMissing True "data/examples/statements/haskell/hslamb"

    BSL.writeFile "data/examples/statements/haskell/hslamb/collatzStep.hss" $ renderStatement (collatzStep :: HSLamb Int Int)
    BSL.writeFile "data/examples/statements/haskell/hslamb/isPalindrome.hss" $ renderStatement (isPalindrome :: HSLamb String Bool)
    BSL.writeFile "data/examples/statements/haskell/hslamb/greetTuple.hss" $ renderStatement (greetTuple :: HSLamb (String, Int) String)
    BSL.writeFile "data/examples/statements/haskell/hslamb/reverseInput.hss" $ renderStatement (revInputProgram :: HSFunc () ())

    createDirectoryIfMissing True "data/examples/statements/haskell/hsmock"

    BSL.writeFile "data/examples/statements/haskell/hsmock/collatzStep.hss" $ renderStatement (collatzStep :: HSMock Int Int)
    BSL.writeFile "data/examples/statements/haskell/hsmock/isPalindrome.hss" $ renderStatement (isPalindrome :: HSMock String Bool)
    BSL.writeFile "data/examples/statements/haskell/hsmock/greetTuple.hss" $ renderStatement (greetTuple :: HSMock (String, Int) String)
    BSL.writeFile "data/examples/statements/haskell/hsmock/reverseInput.hss" $ renderStatement (revInputProgram :: HSMock () ())

    createDirectoryIfMissing True "data/examples/statements/haskell/HSFunc"

    BSL.writeFile "data/examples/statements/haskell/HSFunc/collatzStep.hss" $ renderStatement (collatzStep :: HSFunc Int Int)
    BSL.writeFile "data/examples/statements/haskell/HSFunc/isPalindrome.hss" $ renderStatement (isPalindrome :: HSFunc String Bool)
    BSL.writeFile "data/examples/statements/haskell/HSFunc/greetTuple.hss" $ renderStatement (greetTuple :: HSFunc (String, Int) String)
    BSL.writeFile "data/examples/statements/haskell/HSFunc/reverseInput.hss" $ renderStatement (revInputProgram :: HSFunc () ())

    createDirectoryIfMissing True "data/examples/statements/js/jslamb"

    BSL.writeFile "data/examples/statements/js/jslamb/collatzStep.js" $ renderStatement (collatzStep :: JSLamb Int Int)
    BSL.writeFile "data/examples/statements/js/jslamb/isPalindrome.js" $ renderStatement (isPalindrome :: JSLamb String Bool)
    BSL.writeFile "data/examples/statements/js/jslamb/greetTuple.js" $ renderStatement (greetTuple :: JSLamb (String, Int) String)
    BSL.writeFile "data/examples/statements/js/jslamb/reverseInput.js" $ renderStatement (revInputProgram :: JSLamb () ())

    createDirectoryIfMissing True "data/examples/statements/php/phplamb"

    BSL.writeFile "data/examples/statements/php/phplamb/collatzStep.php" $ renderStatement (collatzStep :: PHPLamb Int Int)
    BSL.writeFile "data/examples/statements/php/phplamb/isPalindrome.php" $ renderStatement (isPalindrome :: PHPLamb String Bool)
    BSL.writeFile "data/examples/statements/php/phplamb/greetTuple.php" $ renderStatement (greetTuple :: PHPLamb (String, Int) String)
    BSL.writeFile "data/examples/statements/php/phplamb/reverseInput.php" $ renderStatement (revInputProgram :: PHPLamb () ())

    -- removeDirectoryRecursive "data/examples/programs"

    createDirectoryIfMissing True "data/examples/programs/haskell/hsfunc"

    BSL.writeFile "data/examples/programs/haskell/hsfunc/collatzStep.hs" $ renderFile (collatzStep :: HSFunc Int Int)
    BSL.writeFile "data/examples/programs/haskell/hsfunc/isPalindrome.hs" $ renderFile (isPalindrome :: HSFunc String Bool)
    BSL.writeFile "data/examples/programs/haskell/hsfunc/greetTuple.hs" $ renderFile (greetTuple :: HSFunc (String, Int) String)
    BSL.writeFile "data/examples/programs/haskell/hsfunc/reverseInput.hs" $ renderFile (revInputProgram :: HSFunc () ())

    createDirectoryIfMissing True "data/examples/programs/js/jsfunc"

    BSL.writeFile "data/examples/programs/js/jsfunc/collatzStep.js" $ renderFile (collatzStep :: JSFunc Int Int)
    BSL.writeFile "data/examples/programs/js/jsfunc/isPalindrome.js" $ renderFile (isPalindrome :: JSFunc String Bool)
    BSL.writeFile "data/examples/programs/js/jsfunc/greetTuple.js" $ renderFile (greetTuple :: JSFunc (String, Int) String)
    BSL.writeFile "data/examples/programs/js/jsfunc/reverseInput.js" $ renderFile (revInputProgram :: JSFunc () ())
