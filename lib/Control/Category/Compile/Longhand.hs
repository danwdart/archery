{-# LANGUAGE Safe #-}

module Control.Category.Compile.Longhand where

import Control.Monad.IO.Class
import Prelude                hiding (id, (.))

class CompileLonghand cat where
    compileLonghand :: (MonadIO m) ⇒ FilePath → cat input output → m ()
