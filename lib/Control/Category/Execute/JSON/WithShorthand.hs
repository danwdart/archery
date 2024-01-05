{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Control.Category.Execute.JSON.WithShorthand where

import Control.Monad.IO.Class
import Data.Aeson

class ExecuteJSONWithShorthand cat where
    executeViaJSONWithShorthand :: (ToJSON input, FromJSON output, MonadIO m) ⇒ cat input output → input → m output
