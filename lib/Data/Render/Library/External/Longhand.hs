{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.Library.External.Longhand (RenderLibraryExternalLonghand(..)) where

import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Code.Generic

class RenderLibraryExternalLonghand a where
    renderLibraryExternalLonghand :: Module → FunctionName → FunctionTypeFrom → FunctionTypeTo → a → BSL.ByteString
