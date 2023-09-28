{-# LANGUAGE Safe #-}

module Control.Category.Primitive.Curried where
-- import Control.Arrow (Kleisli(..))

class PrimitiveCurried cat where
    eqCurried :: Eq a ⇒ cat a (cat a Bool)

instance PrimitiveCurried (->) where
    eqCurried = (==)

{-
instance Monad m => PrimitiveCurried (Kleisli m) where
    eqCurried = Kleisli . (==)
-}
