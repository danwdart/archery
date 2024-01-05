{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Control.Category.Execute.JSON.WithDefinitions where

import Control.Monad.IO.Class
import Data.Aeson

class ExecuteJSONWithDefinitions cat where
    executeViaJSONWithDefinitions :: (ToJSON input, FromJSON output, MonadIO m) ⇒ cat input output → input → m output
