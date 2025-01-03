{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.Library.Internal.Shorthand where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderInternalLibraryShorthand a where
    renderLibraryInternalShorthand :: a → [(FilePath, BSL.ByteString)]
