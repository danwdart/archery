{-# LANGUAGE Safe #-}

module Control.Category.Execute.Haskell.WithShorthand where

import Control.Monad.IO.Class
import Prelude                hiding (id, (.))

class ExecuteHaskellWithShorthand cat where
    executeViaGHCiWithShorthand :: (Show input, Read output, MonadIO m) ⇒ cat input output → input → m output
