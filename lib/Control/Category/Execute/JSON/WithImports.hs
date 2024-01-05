{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Control.Category.Execute.JSON.WithImports where

import Control.Monad.IO.Class
import Data.Aeson

class ExecuteJSONWithImports cat where
    executeViaJSONWithImports :: (ToJSON input, FromJSON output, MonadIO m) ⇒ cat input output → input → m output
