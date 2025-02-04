{-# LANGUAGE Safe #-}

module Data.Function.IsPalindrome (isPalindrome) where

import Control.Category
import Control.Category.Cartesian
import Control.Category.Primitive.Bool
import Control.Category.Primitive.String
import Control.Category.Strong
import Data.Text (Text)
import Prelude                           hiding (id, (.))

isPalindrome ∷ (Category cat, Cartesian cat, Strong cat, PrimitiveString cat, PrimitiveBool cat) ⇒ cat Text Bool
isPalindrome = eq . first' reverseString . copy
