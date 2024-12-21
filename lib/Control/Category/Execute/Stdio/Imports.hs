{-# LANGUAGE Safe #-}

module Control.Category.Execute.Stdio.Imports where

import Control.Monad.IO.Class

class ExecuteStdioImports cat where
    executeStdioImports :: (Show input, Read output, MonadIO m) ⇒ cat () () → input → m output