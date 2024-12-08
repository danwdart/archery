{-# LANGUAGE Safe #-}

module Data.Render.File.Shorthand where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderFileShorthand a where
    renderFileShorthand :: a → BSL.ByteString
