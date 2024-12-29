{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Control.Category.Execute.JS.Shorthand where

import Control.Monad.IO.Class
import Data.Aeson

class ExecuteJSShorthand cat where
    executeJSShorthand :: (MonadIO m) ⇒ cat input output → input → m output
