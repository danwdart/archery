{-# LANGUAGE Safe #-}

module Control.Category.Execute.Stdio.WithShorthand where

import Control.Monad.IO.Class

class ExecuteStdioWithShorthand cat where
    executeViaStdioWithShorthand :: (Show input, Read output, MonadIO m) ⇒ cat () () → input → m output
