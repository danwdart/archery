{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.Primitive.PrimsFile where

import Control.Category.Primitive.File
import Data.Aeson
import Data.Function.Free.Abstract

data PrimFile a b where
    ReadFile :: PrimFile String String
    WriteFile :: PrimFile (String, String) ()

instance PrimitiveFile (FreeFunc PrimFile) where
    readFile' = Lift ReadFile
    writeFile' = Lift WriteFile

instance ToJSON (PrimFile String String) where
    toJSON ReadFile = String "ReadFile"

instance ToJSON (PrimFile (String, String) ()) where
    toJSON WriteFile = String "WriteFile"

-- instance Interpret PrimExtra ()
