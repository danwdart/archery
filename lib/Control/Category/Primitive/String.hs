{-# LANGUAGE Safe #-}

module Control.Category.Primitive.String (PrimitiveString(..)) where

import Control.Arrow (Kleisli (..))

class PrimitiveString cat where
    reverseString :: cat String String

instance PrimitiveString (->) where
    reverseString = reverse

instance Applicative m ⇒ PrimitiveString (Kleisli m) where
    reverseString = Kleisli (pure . reverse)
