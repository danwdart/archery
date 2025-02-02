{-# LANGUAGE Safe #-}

module Control.Category.Execute.Stdio.Shorthand (ExecuteStdioShorthand(..)) where

import Control.Monad.IO.Class

class ExecuteStdioShorthand cat where
    executeStdioShorthand :: (Show input, Read output, MonadIO m) ⇒ cat () () → input → m output
