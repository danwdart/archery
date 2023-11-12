{-# LANGUAGE Safe #-}

module Data.Function.ReverseInput where

import Control.Category
import Control.Category.Primitive.Console
import Control.Category.Primitive.String
import Prelude                            hiding (id, (.))

revInputProgram ∷ (Category cat, PrimitiveConsole cat, PrimitiveString cat) ⇒ cat () ()
revInputProgram = outputString . reverseString . inputString
