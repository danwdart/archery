{-# LANGUAGE Safe #-}

module Control.Category.Execute.Haskell.Imports where

import Control.Monad.IO.Class
import Prelude                hiding (id, (.))

class ExecuteHaskellImports cat where
    executeGHCiImports :: (Show input, Read output, MonadIO m) ⇒ cat input output → input → m output
