{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.Program.Longhand where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderProgramLonghand a where
    renderProgramLonghand :: a → BSL.ByteString
