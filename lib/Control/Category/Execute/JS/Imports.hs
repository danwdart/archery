{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Control.Category.Execute.JS.Imports (ExecuteJSImports(..)) where

import Control.Monad.IO.Class
-- import Data.Aeson

class ExecuteJSImports cat where
    executeJSImports :: MonadIO m ⇒ cat input output → input → m output
