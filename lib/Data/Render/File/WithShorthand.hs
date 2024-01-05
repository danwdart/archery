{-# LANGUAGE Safe #-}

module Data.Render.File.WithShorthand where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderFileWithShorthand a where
    renderFileWithShorthand :: a → BSL.ByteString
