{-# LANGUAGE Safe #-}

module Control.Category.Interpret (Interpret(..)) where

class Interpret cat1 cat2 where
    interpret :: cat1 a b → cat2 a b
