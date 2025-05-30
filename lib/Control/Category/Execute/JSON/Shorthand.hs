{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Control.Category.Execute.JSON.Shorthand (ExecuteJSONShorthand(..)) where

import Control.Monad.IO.Class
import Data.Aeson

class ExecuteJSONShorthand cat where
    executeJSONShorthand :: (ToJSON input, FromJSON output, MonadIO m) ⇒ cat input output → input → m output
