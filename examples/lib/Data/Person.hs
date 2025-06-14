{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Unsafe         #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.Person (Person(..)) where

import Data.Aeson
import GHC.Generics

data Person = Person {
    personName :: String,
    personAge  :: Int
}
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via Generically Person