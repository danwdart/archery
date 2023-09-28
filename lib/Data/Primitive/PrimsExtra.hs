{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.Primitive.PrimsExtra where

import Control.Category.Primitive.Extra
import Data.Aeson
import Data.Function.Free.Abstract
import Data.Text                        qualified as T

data PrimExtra a b where
    IntToString :: PrimExtra Int String
    ConcatString :: PrimExtra (String, String) String
    ConstString :: String -> PrimExtra a String

instance PrimitiveExtra (FreeFunc PrimExtra) where
    intToString = Lift IntToString
    concatString = Lift ConcatString
    constString s = Lift (ConstString s)

instance ToJSON (PrimExtra a b) where
    toJSON IntToString     = String "IntToString"
    toJSON ConcatString    = String "ConcatString"
    toJSON (ConstString s) = Array [ String "ConstString", String (T.pack s) ]

instance FromJSON (PrimExtra Int String) where
    parseJSON (String "IntToString") = pure IntToString
    parseJSON _                      = fail "TypeError: expecting Int -> String"

instance FromJSON (PrimExtra (String, String) String) where
    parseJSON (String "ConcatString") = pure ConcatString
    parseJSON _ = fail "TypeError: expecting (String, String) -> String"

instance FromJSON (PrimExtra () String) where
    parseJSON (Array [ String "ConstString", String s ] ) = pure $ ConstString (T.unpack s)
    parseJSON _ = fail "TypeError: expecting ConstString"

-- instance Interpret PrimExtra ()
