{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main where

-- import Control.Category.Primitive.Person
import Data.Aeson qualified as A
import Data.Function.CollatzStep
import Data.Function.Greet
import Data.Function.IsPalindrome
-- import Data.Function.ReverseInput
import Data.Function.Free.Abstract
-- import Data.Person
import Data.Primitive.Prims
-- import Data.Primitive.PrimsConsole
import Data.Primitive.PrimsExtra
-- import Data.Primitive.PrimPerson
import Data.Yaml qualified as Y

main :: IO ()
main = do
    A.encodeFile "data/examples/jcat/collatzStep.json" (collatzStep :: FreeFunc Prims Int Int)
    Y.encodeFile "data/examples/ycat/collatzStep.yaml" (collatzStep :: FreeFunc Prims Int Int)

    A.encodeFile "data/examples/jcat/isPalindrome.json" (isPalindrome :: FreeFunc Prims String Bool)
    Y.encodeFile "data/examples/ycat/isPalindrome.yaml" (isPalindrome :: FreeFunc Prims String Bool)

    -- A.encodeFile "data/examples/jcat/greetData.json" (greetData :: FreeFunc _ Person String)
    -- Y.encodeFile "data/examples/ycat/greetData.yaml" (greetData :: FreeFunc _ Person String)

    A.encodeFile "data/examples/jcat/greetTuple.json" (greetTuple :: FreeFunc PrimExtra (String, Int) String)
    Y.encodeFile "data/examples/ycat/greetTuple.yaml" (greetTuple :: FreeFunc PrimExtra (String, Int) String)

    -- A.encodeFile "data/examples/jcat/reverseInput.json" (revInputProgram :: FreeFunc PrimsConsole () ())
    -- Y.encodeFile "data/examples/ycat/reverseInput.yaml" (revInputProgram :: FreeFunc PrimsConsole () ())