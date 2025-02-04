{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe #-}

module Control.Category.Primitive.File (PrimitiveFile(..)) where

import Control.Arrow          (Kleisli (..))
import Control.Monad.IO.Class
import Data.ByteString.Char8 (ByteString)
import System.OsPath.Types
import System.File.OsPath qualified as OSP

class PrimitiveFile cat where
    readFile' :: cat OsPath ByteString
    writeFile' :: cat (OsPath, ByteString) ()

instance (MonadIO m) ⇒ PrimitiveFile (Kleisli m) where
    readFile' = Kleisli $ liftIO . OSP.readFile'
    writeFile' = Kleisli $ liftIO . uncurry OSP.writeFile'

