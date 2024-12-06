{-# LANGUAGE Safe #-}

module Data.Render.Statement.WithLonghand where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderStatementWithLonghand a where
    renderStatementWithLonghand :: a → BSL.ByteString
