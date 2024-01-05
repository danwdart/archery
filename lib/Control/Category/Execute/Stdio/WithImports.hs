{-# LANGUAGE Safe #-}

module Control.Category.Execute.Stdio.WithImports where

import Control.Monad.IO.Class

class ExecuteStdioWithImports cat where
    executeViaStdioWithImports :: (Show input, Read output, MonadIO m) ⇒ cat () () → input → m output
