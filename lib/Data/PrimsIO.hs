{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.PrimsIO (PrimsIO(..)) where

import Control.Category.Interpret
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Control.Category.Primitive.File
import Control.Category.Primitive.String
import Data.Aeson
import Data.ByteString.Char8              (ByteString)
import Data.Function.Free.Abstract
import Data.Text                          (Text)
import System.OsPath.Types                (OsPath)

data PrimsIO a b where
    ReverseString :: PrimsIO Text Text
    Equal :: Eq a => PrimsIO (a, a) Bool
    OutputString :: PrimsIO Text ()
    InputString :: PrimsIO () Text
    IntToString :: PrimsIO Int Text
    ConcatString :: PrimsIO (Text, Text) Text
    ConstString :: Text -> PrimsIO a Text
    ReadFile :: PrimsIO OsPath ByteString
    WriteFile :: PrimsIO (OsPath, ByteString) ()

deriving instance Eq (PrimsIO a b)
deriving instance Show (PrimsIO a b)

instance ToJSON (PrimsIO a b) where
    toJSON IntToString     = String "IntToString"
    toJSON ConcatString    = String "ConcatString"
    toJSON (ConstString s) = Array [ String "ConstString", String s ]
    toJSON InputString     = String "InputString"
    toJSON OutputString    = String "OutputString"
    toJSON WriteFile       = String "WriteFile"
    toJSON Equal           = String "Equal"
    toJSON ReverseString   = String "ReverseString"
    toJSON ReadFile        = String "ReadFile"

instance FromJSON (PrimsIO Text Text) where
    parseJSON (String "ReverseString") = pure ReverseString
    parseJSON _ = fail "TypeError: expecting String -> String"

instance Eq a ⇒ FromJSON (PrimsIO (a, a) Bool) where
    parseJSON (String "Equal") = pure Equal
    parseJSON _                = fail "TypeError: expecting (a, a) -> Bool"

instance FromJSON (PrimsIO Int Text) where
    parseJSON (String "IntToString") = pure IntToString
    parseJSON _                      = fail "TypeError: expecting Int -> String"

instance FromJSON (PrimsIO (Text, Text) Text) where
    parseJSON (String "ConcatString") = pure ConcatString
    parseJSON _ = fail "TypeError: expecting (String, String) -> String"

instance FromJSON (PrimsIO () Text) where
    parseJSON (Array [ String "ConstString", String s ] ) = pure $ ConstString s
    parseJSON _ = fail "TypeError: expecting ConstString"

instance PrimitiveBool (FreeFunc PrimsIO) where
    eq = Lift Equal

instance PrimitiveString (FreeFunc PrimsIO) where
    reverseString = Lift ReverseString

instance PrimitiveConsole (FreeFunc PrimsIO) where
    outputString = Lift OutputString
    inputString = Lift InputString

instance PrimitiveExtra (FreeFunc PrimsIO) where
    intToString = Lift IntToString
    concatString = Lift ConcatString
    constString s = Lift (ConstString s)

instance PrimitiveFile (FreeFunc PrimsIO) where
    readFile' = Lift ReadFile
    writeFile' = Lift WriteFile

instance (PrimitiveBool cat, PrimitiveString cat, PrimitiveConsole cat, PrimitiveExtra cat, PrimitiveFile cat) ⇒ Interpret PrimsIO cat where
    interpret Equal           = eq
    interpret ReverseString   = reverseString
    interpret OutputString    = outputString
    interpret InputString     = inputString
    interpret IntToString     = intToString
    interpret ConcatString    = concatString
    interpret (ConstString s) = constString s
    interpret ReadFile        = readFile'
    interpret WriteFile       = writeFile'
