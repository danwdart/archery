{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module Data.Function.Greet (greetWith, greetTuple) where

import Control.Category
import Control.Category.Cartesian
import Control.Category.Primitive.Extra
import Control.Category.Strong
import Control.Category.Symmetric
-- import Data.Person
import Prelude                          hiding (id, (.))

greetWith ∷ (Category cat, Cartesian cat, Strong cat, Symmetric cat, PrimitiveExtra cat) ⇒ cat a String → cat a Int → cat a String
greetWith nameSelector ageSelector = concatString . second' intToString . first' concatString . first' swap . reassoc . second' (second' ageSelector) . second' (first' nameSelector) . first' (constString " is ") . second' copy . copy

-- greetData ∷ (Category cat, Cartesian cat, Strong cat, Symmetric cat) ⇒ cat Person String
-- greetData = greetWith (get "name") (get "age")

greetTuple ∷ (Category cat, Cartesian cat, Strong cat, Symmetric cat, PrimitiveExtra cat) ⇒ cat (String, Int) String
greetTuple = greetWith fst' snd'
