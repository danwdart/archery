{-# LANGUAGE Safe #-}

module Control.Category.Execute.Stdio.Longhand where

import Control.Monad.IO.Class

class ExecuteStdioLonghand cat where
    executeStdioLonghand :: (Show input, Read output, MonadIO m) ⇒ cat () () → input → m output