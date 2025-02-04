{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module Data.Function.Greet (greetWith, greetTuple) where

import Control.Category
import Control.Category.Cartesian
import Control.Category.Primitive.Extra
import Control.Category.Strong
import Control.Category.Symmetric
-- import Data.Person
import Data.Text (Text)
import Prelude                          hiding (id, (.))

greetWith ∷ (Category cat, Cartesian cat, Strong cat, Symmetric cat, PrimitiveExtra cat) ⇒ cat a Text → cat a Int → cat a Text
greetWith nameSelector ageSelector = concatString . second' intToString . first' concatString . first' swap . reassoc . second' (second' ageSelector) . second' (first' nameSelector) . first' (constString " is ") . second' copy . copy

-- greetData ∷ (Category cat, Cartesian cat, Strong cat, Symmetric cat) ⇒ cat Person Text
-- greetData = greetWith (get "name") (get "age")

greetTuple ∷ (Category cat, Cartesian cat, Strong cat, Symmetric cat, PrimitiveExtra cat) ⇒ cat (Text, Int) Text
greetTuple = greetWith fst' snd'
