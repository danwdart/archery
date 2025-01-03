{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.Library.External.Shorthand where

import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Code.Generic

class RenderLibraryExternalShorthand a where
    renderLibraryExternalShorthand :: Module → FunctionName → FunctionTypeFrom → FunctionTypeTo → a → BSL.ByteString
