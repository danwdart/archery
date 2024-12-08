{-# LANGUAGE Safe #-}

module Data.Render.Statement.Shorthand where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderStatementShorthand a where
    renderStatementShorthand :: a → BSL.ByteString
