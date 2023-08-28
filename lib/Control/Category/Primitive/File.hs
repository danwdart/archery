{-# LANGUAGE Safe #-}

module Control.Category.Primitive.File where

import Control.Arrow (Kleisli (..))
import Control.Monad.IO.Class

class PrimitiveFile cat where
    readFile' :: cat String String
    writeFile' :: cat (String, String) ()

instance (MonadIO m) ⇒ PrimitiveFile (Kleisli m) where
    readFile' = Kleisli $ liftIO . readFile
    writeFile' = Kleisli $ liftIO . uncurry writeFile

