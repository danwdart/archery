{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Control.Category.Execute.JSON.Imports where

import Control.Monad.IO.Class
import Data.Aeson

class ExecuteJSONImports cat where
    executeJSONImports :: (ToJSON input, FromJSON output, MonadIO m) ⇒ cat input output → input → m output
