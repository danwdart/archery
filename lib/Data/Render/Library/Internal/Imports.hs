{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.Library.Internal.Imports where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderInternalLibraryImports a where
    renderLibraryInternalImports :: a → [(FilePath, BSL.ByteString)]
