{-# LANGUAGE Safe #-}

module Control.Category.Compile.Shorthand (CompileShorthand(..)) where

import Control.Monad.IO.Class
import Prelude                hiding (id, (.))

class CompileShorthand cat where
    compileShorthand :: (MonadIO m) ⇒ FilePath → cat input output → m ()
