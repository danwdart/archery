{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.Library.External.Imports (RenderLibraryExternalImports(..)) where

import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Code.Generic

class RenderLibraryExternalImports a where
    renderLibraryExternalImports :: Module → FunctionName → FunctionTypeFrom → FunctionTypeTo → a → BSL.ByteString
