{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.Library.Imports where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderLibraryImports a where
    renderLibraryImports :: a → [(FilePath, BSL.ByteString)]
