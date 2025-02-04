{-# LANGUAGE Safe #-}

module Control.Category.Primitive.String (PrimitiveString(..)) where

import Control.Arrow (Kleisli (..))
import Data.Text (Text)
import Data.Text qualified as T

class PrimitiveString cat where
    reverseString :: cat Text Text

instance PrimitiveString (->) where
    reverseString = T.reverse

instance Applicative m ⇒ PrimitiveString (Kleisli m) where
    reverseString = Kleisli (pure . T.reverse)
