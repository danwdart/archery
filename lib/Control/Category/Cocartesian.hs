{-# LANGUAGE Safe #-}

module Control.Category.Cocartesian (Cocartesian(..)) where

import Control.Arrow (Kleisli (..))

class Cocartesian cat where
    injectL :: cat a (Either a b)
    injectR :: cat a (Either b a)
    unify :: cat (Either a a) a
    tag :: cat (Bool, a) (Either a a)

instance Cocartesian (->) where
    injectL = Left
    injectR = Right
    unify = \case
        Left a  -> a
        Right a -> a
    tag (False, a) = Left a
    tag (True, a)  = Right a

instance Applicative m ⇒ Cocartesian (Kleisli m) where
    injectL = Kleisli $ pure . Left
    injectR = Kleisli $ pure . Right
    unify = Kleisli $ pure . unify
    tag = Kleisli $ pure . tag
