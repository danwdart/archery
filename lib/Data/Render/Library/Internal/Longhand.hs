{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.Library.Internal.Longhand where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderInternalLibraryLonghand a where
    renderLibraryInternalLonghand :: a → [(FilePath, BSL.ByteString)]
