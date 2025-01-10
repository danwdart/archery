{-# LANGUAGE Safe #-}

module Control.Category.Execute.Haskell.Longhand (ExecuteHaskellLonghand(..)) where

import Control.Monad.IO.Class
import Prelude                hiding (id, (.))

class ExecuteHaskellLonghand cat where
    executeGHCiLonghand :: (Show input, Read output, MonadIO m) ⇒ cat input output → input → m output
