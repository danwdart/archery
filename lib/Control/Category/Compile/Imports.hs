{-# LANGUAGE Safe #-}

module Control.Category.Compile.Imports (CompileImports(..)) where

import Control.Monad.IO.Class
import Prelude                hiding (id, (.))

class CompileImports cat where
    compileImports :: (MonadIO m) ⇒ FilePath → cat input output → m ()
