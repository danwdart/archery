{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.File.Imports where

import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Code.Generic

class RenderFileImports a where
    renderFileImports :: Module → FunctionName → a → BSL.ByteString -- TODO potentially more files?
