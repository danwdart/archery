{-# LANGUAGE Safe #-}

module Data.Render.File where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderFile a where
    renderFile :: a → BSL.ByteString
