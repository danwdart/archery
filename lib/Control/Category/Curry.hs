{-# LANGUAGE Safe #-}

module Control.Category.Curry (Curry(..)) where

import Control.Arrow (Kleisli(..))

class Curry cat where
    curry' :: cat (cat (a, b) c) (cat a (cat b c))
    uncurry' :: cat (cat a (cat b c)) (cat (a, b) c)

instance Curry (->) where
    curry' = curry
    uncurry' = uncurry

instance Monad m => Curry (Kleisli m) where
    curry' = Kleisli (\(Kleisli abmc) -> pure (Kleisli (\a -> pure (Kleisli (\b -> abmc (a, b))))))
    uncurry' = Kleisli (\(Kleisli amkmbc) -> pure (Kleisli (\(a, b) -> amkmbc a >>= \f -> runKleisli f b)))
