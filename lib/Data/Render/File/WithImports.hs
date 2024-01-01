{-# LANGUAGE Safe #-}

module Data.Render.File.WithImports where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderFileWithImports a where
    renderFileWithImports :: a → BSL.ByteString
