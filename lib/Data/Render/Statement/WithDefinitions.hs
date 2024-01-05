{-# LANGUAGE Safe #-}

module Data.Render.Statement.WithDefinitions where

import Data.ByteString.Lazy.Char8 qualified as BSL

class RenderStatementWithDefinitions a where
    renderStatementWithDefinitions :: a → BSL.ByteString
