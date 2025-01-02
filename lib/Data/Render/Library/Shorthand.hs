{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.Library.Shorthand where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderLibraryShorthand a where
    renderLibraryShorthand :: a → [(FilePath, BSL.ByteString)]
