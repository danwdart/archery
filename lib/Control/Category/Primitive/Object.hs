{-# LANGUAGE Safe #-}

module Control.Category.Primitive.Object where

import Control.Arrow (Kleisli (..))

class PrimitiveBool cat where
    get :: String -> cat a b

instance PrimitiveBool (->) where
    get = uncurry (==)

instance Monad m ⇒ PrimitiveBool (Kleisli m) where
    eq = Kleisli (pure . eq)
