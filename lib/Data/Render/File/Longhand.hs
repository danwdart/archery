{-# LANGUAGE Safe #-}

module Data.Render.File.Longhand where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderFileLonghand a where
    renderFileLonghand :: a → BSL.ByteString
