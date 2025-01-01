{-# LANGUAGE Safe #-}

module Control.Category.Primitive.Console where

import Control.Arrow (Kleisli (..))
import Control.Monad.IO.Class

class PrimitiveConsole cat where
    outputString :: cat String ()
    inputString :: cat () String
    --konst :: b -> cat a b

instance MonadIO m => PrimitiveConsole (Kleisli m) where
    outputString = Kleisli (liftIO . putStrLn)
    inputString = Kleisli (const . liftIO $ getLine)
