{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.Prims (Prims(..)) where

import Control.Category.Interpret
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Control.Category.Primitive.File
import Control.Category.Primitive.String
import Data.Aeson
import Data.Function.Free.Abstract
import Data.Text                          qualified as T

data Prims a b where
    ReverseString :: Prims String String
    Equal :: Eq a => Prims (a, a) Bool
    OutputString :: Prims String ()
    InputString :: Prims () String
    IntToString :: Prims Int String
    ConcatString :: Prims (String, String) String
    ConstString :: String -> Prims a String
    ReadFile :: Prims String String
    WriteFile :: Prims (String, String) ()

deriving instance Show (Prims a b)

instance ToJSON (Prims a b) where
-- instance ToJSON (Prims Int String) where
    toJSON IntToString     = String "IntToString"
--     toJSON _ = error "Prims Int STring?"

-- instance ToJSON (Prims (String, String) String) where
    toJSON ConcatString    = String "ConcatString"
 --    toJSON _ = error "Prims (String, String) String?"

-- instance ToJSON (Prims () String) where
    toJSON (ConstString s) = Array [ String "ConstString", String (T.pack s) ]
    toJSON InputString     = String "InputString"
 --    toJSON _ = error "Prims () String?"

-- instance ToJSON (Prims String ()) where
    toJSON OutputString    = String "OutputString"

-- instance ToJSON (Prims (String, String) ()) where
    toJSON WriteFile       = String "WriteFile"

-- instance ToJSON (Prims (String, String) Bool) where
    toJSON Equal           = String "Equal"

-- instance ToJSON (Prims String String) where
    -- instance ToJSON (Prims String String) where
    toJSON ReverseString   = String "ReverseString"
    toJSON ReadFile        = String "ReadFile"

instance FromJSON (Prims String String) where
    parseJSON (String "ReverseString") = pure ReverseString
    parseJSON _ = fail "TypeError: expecting String -> String"

instance Eq a ⇒ FromJSON (Prims (a, a) Bool) where
    parseJSON (String "Equal") = pure Equal
    parseJSON _                = fail "TypeError: expecting (a, a) -> Bool"

instance FromJSON (Prims Int String) where
    parseJSON (String "IntToString") = pure IntToString
    parseJSON _                      = fail "TypeError: expecting Int -> String"

instance FromJSON (Prims (String, String) String) where
    parseJSON (String "ConcatString") = pure ConcatString
    parseJSON _ = fail "TypeError: expecting (String, String) -> String"

instance FromJSON (Prims () String) where
    parseJSON (Array [ String "ConstString", String s ] ) = pure $ ConstString (T.unpack s)
    parseJSON _ = fail "TypeError: expecting ConstString"

instance PrimitiveBool (FreeFunc Prims) where
    eq = Lift Equal

instance PrimitiveString (FreeFunc Prims) where
    reverseString = Lift ReverseString

instance PrimitiveConsole (FreeFunc Prims) where
    outputString = Lift OutputString
    inputString = Lift InputString

instance PrimitiveExtra (FreeFunc Prims) where
    intToString = Lift IntToString
    concatString = Lift ConcatString
    constString s = Lift (ConstString s)

instance PrimitiveFile (FreeFunc Prims) where
    readFile' = Lift ReadFile
    writeFile' = Lift WriteFile

instance (PrimitiveBool cat, PrimitiveString cat, PrimitiveConsole cat, PrimitiveExtra cat, PrimitiveFile cat) ⇒ Interpret Prims cat where
    interpret Equal           = eq
    interpret ReverseString   = reverseString
    interpret OutputString    = outputString
    interpret InputString     = inputString
    interpret IntToString     = intToString
    interpret ConcatString    = concatString
    interpret (ConstString s) = constString s
    interpret ReadFile        = readFile'
    interpret WriteFile       = writeFile'
