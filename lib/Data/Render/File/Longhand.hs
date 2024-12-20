{-# LANGUAGE Unsafe #-}

module Data.Render.File.Longhand where

import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Code.Generic

class RenderFileLonghand a where
    renderFileLonghand :: Module → FunctionName → a → BSL.ByteString
