{-# LANGUAGE Safe #-}

module Control.Category.LambdaCalculus.SKI (SKI(..)) where

class SKI where
    s :: cat (cat a b) (cat b a)
    k :: cat (cat a b) a
    i :: cat a a
