{-# LANGUAGE Safe #-}

module Data.Render.Statement where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderStatement a where
    renderStatement :: a → BSL.ByteString
