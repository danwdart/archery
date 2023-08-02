{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wwarn -Wno-unsafe #-}

module Main where

import Control.Category
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8  qualified as BSL
import Data.Function.Free.Abstract
import Data.Primitive.Prims
import Data.Yaml                   qualified as Y
import Prelude                     hiding (id, (.))
import System.Executable

-- | Compiles a category from YAML category file to a Haskell function source file.
main ∷ IO ()
main = readToWrite ((pure . encode :: FreeFunc Prims String String → IO BSL.ByteString)
    <=<
    (Y.decodeThrow . BSL.toStrict :: BSL.ByteString → IO (FreeFunc Prims String String)))
