{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Data.Render.File.Shorthand where

import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Code.Generic

class RenderFileShorthand a where
    renderFileShorthand :: Module → FunctionName → FunctionTypeFrom -> FunctionTypeTo -> a → BSL.ByteString
