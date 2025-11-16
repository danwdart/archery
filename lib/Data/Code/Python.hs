{-# LANGUAGE Unsafe               #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.Code.Python (Python(..)) where

import Data.Code.Generic

newtype Python a b = Python {
    _code :: Code a b
} deriving stock (Eq, Show)