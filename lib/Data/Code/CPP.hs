{-# LANGUAGE Unsafe               #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.Code.CPP (CPP(..)) where

import Data.Code.Generic

newtype CPP a b = CPP {
    _code :: Code a b
} deriving stock (Eq, Show)