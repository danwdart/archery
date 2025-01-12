{-# LANGUAGE Safe #-}

module Control.Category.Primitive.Curried (PrimitiveCurried(..)) where
import Control.Arrow (Kleisli (..))

class PrimitiveCurried cat where
    eqCurried :: Eq a ⇒ cat a (cat a Bool)

instance PrimitiveCurried (->) where
    eqCurried = (==)

instance Applicative m ⇒ PrimitiveCurried (Kleisli m) where
    eqCurried = Kleisli (\a -> pure (Kleisli (\b -> pure (a == b))))
