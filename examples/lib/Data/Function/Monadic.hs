{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

module Data.Function.Monadic (monadicIOThing, monadicThing) where

import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Control.Arrow
import Data.Text (Text)
import Control.Monad
import Control.Category.Primitive.Bool

monadicIOThing :: (PrimitiveConsole cat, ArrowApply cat, Monad (ArrowMonad cat)) => cat () ()
monadicIOThing = do
    askName <- constString "Please enter your name."
    outputString askName
    name <- inputString
    outputString (constString "And your password:")
    pwd <- inputString
    when (eq (str, "pwd123")) $ outputString (concatString (concatString (constString "Wonderful! You're in, ", name), constString "!"))
    unless (eq (str, "pwd123")) $ outputString (concatString (concatString (constString "Oh no! You're wrong, ", name), constString "!"))

monadicThing :: (Monad (ArrowMonad cat)) => cat () Text
monadicThing = do
    pwd <- constString "pwd123"
    end <- constString "!"
    when (eq (pwd, "pwd123")) $ do
        start <- constString "Wonderful! You're in, "
        let middle = concatString (start, pwd)
        let ret = concatString (middle, end)
        pure ret
    unless (eq (str, "pwd123")) $ do
        pure $ concatString (concatString (constString "Oh no! You're wrong, ", name), constString "!")
