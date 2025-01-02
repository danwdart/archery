{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.Program.Shorthand where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderProgramShorthand a where
    renderProgramShorthand :: a → BSL.ByteString
