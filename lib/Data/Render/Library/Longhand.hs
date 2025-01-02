{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.Library.Longhand where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderLibraryLonghand a where
    renderLibraryLonghand :: a → [(FilePath, BSL.ByteString)]
