{-# LANGUAGE Safe #-}

module Control.Category.Execute.Haskell.WithImports where

import Control.Monad.IO.Class
import Prelude                hiding (id, (.))

class ExecuteHaskellWithImports cat where
    executeViaGHCiWithImports :: (Show input, Read output, MonadIO m) ⇒ cat input output → input → m output
