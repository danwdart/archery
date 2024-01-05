{-# LANGUAGE Safe #-}

module Control.Category.Execute.Stdio.WithDefinitions where

import Control.Monad.IO.Class

class ExecuteStdioWithDefinitions cat where
    executeViaStdioWithDefinitions :: (Show input, Read output, MonadIO m) ⇒ cat () () → input → m output
