{-# LANGUAGE Safe #-}

module Data.Render.File.WithDefinitions where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderFileWithDefinitions a where
    renderFileWithDefinitions :: a → BSL.ByteString
