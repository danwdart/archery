{-# LANGUAGE Unsafe #-}
{-# OPTIONS -Wno-unsafe -Wno-safe #-}

module Data.Function.CollatzStep (collatzStep) where

import Control.Category
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Numeric
import Control.Category.Primitive.Bool
import Control.Category.Strong
import Data.Aeson
import Data.Function.Utilities
import Prelude                         hiding (id, (.))

collatzStep ∷ forall cat n. (Category cat, Numeric cat, Cartesian cat, Cocartesian cat, Choice cat, Strong cat, PrimitiveBool cat, Integral n, Show n, ToJSON n) ⇒ cat n n
collatzStep = unify . (onOdds +++ onEvens) . matchOn isEven where
    onOdds ∷ cat n n
    onOdds = strong add (num 1) . strong mult (num 3)

    onEvens ∷ cat n n
    onEvens = strong div' (num 2)

    isEven ∷ cat n Bool
    isEven = strong eq (num 0) . mod2

    mod2 ∷ cat n n
    mod2 = strong mod' (num 2)

    matchOn ∷ cat a Bool → cat a (Either a a)
    matchOn predicate = tag . first' predicate . copy
