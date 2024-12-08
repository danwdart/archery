{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Control.Category.Execute.JSON.Longhand where

import Control.Monad.IO.Class
import Data.Aeson

class ExecuteJSONLonghand cat where
    executeJSONLonghand :: (ToJSON input, FromJSON output, MonadIO m) ⇒ cat input output → input → m output
