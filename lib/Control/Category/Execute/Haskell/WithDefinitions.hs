{-# LANGUAGE Safe #-}

module Control.Category.Execute.Haskell.WithDefinitions where

import Control.Monad.IO.Class
import Prelude                hiding (id, (.))

class ExecuteHaskellWithDefinitions cat where
    executeViaGHCiWithDefinitions :: (Show input, Read output, MonadIO m) ⇒ cat input output → input → m output
