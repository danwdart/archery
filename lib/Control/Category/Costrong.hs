{-# LANGUAGE Safe #-}

module Control.Category.Costrong (Costrong(..)) where

import Control.Arrow     (Kleisli (..))
-- import Control.Monad
import Control.Monad.Fix

class Costrong cat where
    unfirst :: cat (a, c) (b, c) → cat a b

-- Like Costrong or ArrowLoop
instance Costrong (->) where
    unfirst :: ((a, c) → (b, c)) → a → b
    unfirst f a = let (b, c) = f (a, c) in b

instance MonadFix m ⇒ Costrong (Kleisli m) where
    unfirst (Kleisli f) = Kleisli (fmap fst . mfix . f')
        where
            f' x y = f (x, snd y)
