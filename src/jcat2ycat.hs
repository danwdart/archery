{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wwarn -Wno-unsafe #-}

module Main (main) where

import Control.Category
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Function.Free.Abstract
#if !(MIN_VERSION_aeson(2,1,2))
import Data.Maybe
#endif
import Data.Prims
import Data.Yaml qualified as Y
import System.Executable
import Prelude hiding ((.), id)

-- | Compiles a category from YAML category file to a Haskell function source file.
main ∷ IO ()
main = readToWrite ((pure . BSL.fromStrict . Y.encode :: FreeFunc Prims () () -> IO BSL.ByteString)
    <=<
#if MIN_VERSION_aeson(2,1,2)
    (throwDecode :: BSL.ByteString -> IO (FreeFunc Prims () ())))
#else
    (fromJust . decode :: BSL.ByteString -> IO (FreeFunc Prims () ())))
#endif