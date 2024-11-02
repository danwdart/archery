{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Safe               #-}

module Data.Function.AskName where

import Control.Category
import Control.Category.Cartesian
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Control.Category.Strong
import Prelude                            hiding (id, (.))

askName ∷ (Category cat, Cartesian cat, PrimitiveConsole cat, PrimitiveExtra cat, Strong cat) ⇒ cat () ()
askName = outputString . concatString . first' (constString "Hello, ") . copy . inputString . outputString . constString "What is your name?\\n"
