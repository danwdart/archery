{-# LANGUAGE Safe #-}

module Control.Category.Execute.Haskell.Shorthand where

import Control.Monad.IO.Class
import Prelude                hiding (id, (.))

class ExecuteHaskellShorthand cat where
    executeGHCiShorthand :: (Show input, Read output, MonadIO m) ⇒ cat input output → input → m output
