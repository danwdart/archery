{-# LANGUAGE Safe #-}

module Data.Render.File.Imports where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderFileImports a where
    renderFileImports :: a → BSL.ByteString
