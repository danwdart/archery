{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Control.Category.Execute.JS.Imports where

import Control.Monad.IO.Class
import Data.Aeson

class ExecuteJSImports cat where
    executeJSImports :: MonadIO m ⇒ cat input output → input → m output
