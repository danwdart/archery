{-# LANGUAGE Safe #-}

module Data.Render.Statement.Longhand (RenderStatementLonghand(..)) where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderStatementLonghand a where
    renderStatementLonghand :: a → BSL.ByteString
