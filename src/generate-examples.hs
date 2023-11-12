{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main (main) where

import Data.Aeson                  qualified as A
import Data.Function.CollatzStep
import Data.Function.Free.Abstract
import Data.Function.Greet
import Data.Function.IsPalindrome
import Data.Function.ReverseInput
-- import Data.Person
import Data.Prims
import Data.Yaml                   qualified as Y

main ∷ IO ()
main = do
    A.encodeFile "data/examples/jcat/collatzStep.json" (collatzStep :: FreeFunc Prims Int Int)
    Y.encodeFile "data/examples/ycat/collatzStep.yaml" (collatzStep :: FreeFunc Prims Int Int)

    A.encodeFile "data/examples/jcat/isPalindrome.json" (isPalindrome :: FreeFunc Prims String Bool)
    Y.encodeFile "data/examples/ycat/isPalindrome.yaml" (isPalindrome :: FreeFunc Prims String Bool)

    -- A.encodeFile "data/examples/jcat/greetData.json" (greetData :: FreeFunc Prims Person String)
    -- Y.encodeFile "data/examples/ycat/greetData.yaml" (greetData :: FreeFunc Prims Person String)

    A.encodeFile "data/examples/jcat/greetTuple.json" (greetTuple :: FreeFunc Prims (String, Int) String)
    Y.encodeFile "data/examples/ycat/greetTuple.yaml" (greetTuple :: FreeFunc Prims (String, Int) String)

    A.encodeFile "data/examples/jcat/reverseInput.json" (revInputProgram :: FreeFunc Prims () ())
    Y.encodeFile "data/examples/ycat/reverseInput.yaml" (revInputProgram :: FreeFunc Prims () ())
