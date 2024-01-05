{-# LANGUAGE Safe #-}

module Data.Render.Statement.WithShorthand where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderStatementWithShorthand a where
    renderStatementWithShorthand :: a → BSL.ByteString
