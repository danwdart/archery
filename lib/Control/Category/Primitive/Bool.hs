{-# LANGUAGE Safe #-}

module Control.Category.Primitive.Bool (PrimitiveBool(..)) where

import Control.Arrow (Kleisli (..))

class PrimitiveBool cat where
    eq :: Eq a ⇒ cat (a, a) Bool

instance PrimitiveBool (->) where
    eq = uncurry (==)

instance Applicative m ⇒ PrimitiveBool (Kleisli m) where
    eq = Kleisli (pure . eq)
