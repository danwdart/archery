{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Control.Category.Execute.JS.Longhand where

import Control.Monad.IO.Class
import Data.Aeson

class ExecuteJSLonghand cat where
    executeJSLonghand :: (MonadIO m) ⇒ cat input output → input → m output
