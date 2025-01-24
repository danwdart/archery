{-# LANGUAGE Safe #-}

module Control.Category.Primitive.Console (PrimitiveConsole(..)) where

import Control.Arrow          (Kleisli (..))
import Control.Monad.IO.Class
import Data.Text              (Text)
import Data.Text.IO           qualified as TIO

class PrimitiveConsole cat where
    outputString :: cat Text ()
    inputString :: cat () Text
    --konst :: b -> cat a b

instance MonadIO m ⇒ PrimitiveConsole (Kleisli m) where
    outputString = Kleisli (liftIO . TIO.putStrLn)
    inputString = Kleisli (const . liftIO $ TIO.getLine)
