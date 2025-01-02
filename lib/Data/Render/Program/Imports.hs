{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.Program.Imports where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderProgramImports a where
    renderProgramImports :: a → BSL.ByteString -- TODO potentially more files?
