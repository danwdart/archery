{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-safe -Wno-unsafe #-}
-- TODO maybe make this into a library?
module Orphans where

import Data.Aeson
import Data.Aeson.Types
import Data.Text qualified as T
import System.OsPath

instance FromJSON OsPath where
    -- No MonadThrow for Aeson.Parser here - todo safer
    parseJSON (String a) = pure . unsafeEncodeUtf $ T.unpack a
    parseJSON invalid = typeMismatch "String | Number" invalid

instance ToJSON OsPath where
    toJSON = String . T.pack . show