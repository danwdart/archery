{-# LANGUAGE Safe #-}

module Control.Category.Primitive.String where

import Control.Arrow (Kleisli (..))

class PrimitiveString cat where
    reverseString :: cat String String

instance PrimitiveString (->) where
    reverseString = reverse

instance Monad m ⇒ PrimitiveString (Kleisli m) where
    reverseString = Kleisli (pure . reverseString)
