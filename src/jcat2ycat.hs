{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wwarn -Wno-unsafe -Wno-unused-imports #-}

module Main (main) where

import Control.Category
import Control.Monad
import Data.Aeson
import Data.Aeson.Compat
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Function.Free.Abstract
import Data.Prims
import Data.Yaml qualified as Y
import System.Executable
import Prelude hiding ((.), id)

-- | Compiles a category from YAML category file to a Haskell function source file.
main ∷ IO ()
main = readToWrite ((pure . BSL.fromStrict . Y.encode :: FreeFunc Prims () () -> IO BSL.ByteString)
    <=<
    (throwDecode :: BSL.ByteString -> IO (FreeFunc Prims () ())))
