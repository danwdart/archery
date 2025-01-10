{-# LANGUAGE Safe #-}

module Control.Category.Execute.Haskell.Shorthand (ExecuteHaskellShorthand(..)) where

import Control.Monad.IO.Class
import Prelude                hiding (id, (.))

class ExecuteHaskellShorthand cat where
    executeGHCiShorthand :: (Show input, Read output, MonadIO m) ⇒ cat input output → input → m output
