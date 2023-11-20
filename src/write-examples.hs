{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main (main) where

-- import Control.Category.Interpret
-- import Data.Aeson                  qualified as A
-- import Data.ByteString.Char8      qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Code.Haskell.Func
import Data.Code.Haskell.Lamb
import Data.Code.Haskell.Mock
import Data.Code.Haskell.Program
import Data.Code.JS.Lamb
import Data.Code.PHP.Lamb
import Data.Function.CollatzStep
-- import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
-- import Data.Person
-- import Data.Prims
import Data.Render.File
import Data.Render.Statement
import System.Directory

main ∷ IO ()
main = do
    removeDirectoryRecursive "data/examples/statements"

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

    createDirectoryIfMissing True "data/examples/statements/haskell/hsprog"

    BSL.writeFile "data/examples/statements/haskell/hsprog/collatzStep.hss" $ renderStatement (collatzStep :: HSProg Int Int)
    BSL.writeFile "data/examples/statements/haskell/hsprog/isPalindrome.hss" $ renderStatement (isPalindrome :: HSProg String Bool)
    BSL.writeFile "data/examples/statements/haskell/hsprog/greetTuple.hss" $ renderStatement (greetTuple :: HSProg (String, Int) String)
    BSL.writeFile "data/examples/statements/haskell/hsprog/reverseInput.hss" $ renderStatement (revInputProgram :: HSProg () ())

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

    removeDirectoryRecursive "data/examples/programs"

    createDirectoryIfMissing True "data/examples/programs/haskell/hsprog"

    BSL.writeFile "data/examples/programs/haskell/hsprog/collatzStep.hs" $ renderFile (collatzStep :: HSProg Int Int)
    BSL.writeFile "data/examples/programs/haskell/hsprog/isPalindrome.hs" $ renderFile (isPalindrome :: HSProg String Bool)
    BSL.writeFile "data/examples/programs/haskell/hsprog/greetTuple.hs" $ renderFile (greetTuple :: HSProg (String, Int) String)
    BSL.writeFile "data/examples/programs/haskell/hsprog/reverseInput.hs" $ renderFile (revInputProgram :: HSProg () ())