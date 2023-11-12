{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wwarn -Wno-unsafe #-}

module Main (main) where

import Control.Category
import Control.Category.Interpret
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Code.Haskell.Lamb
import Data.Function.Free.Abstract
#if !(MIN_VERSION_aeson(2,1,2))
import Data.Maybe
#endif
import Data.Prims
import Data.Render.Statement
import System.Executable
import Prelude hiding ((.), id)

-- | Compiles a category from YAML category file to a Haskell function source file.
main ∷ IO ()
main = readToWrite (\bs ->
    (pure . renderStatement :: HSLamb () () -> IO BSL.ByteString) =<<
    (pure . interpret :: FreeFunc Prims () () -> IO (HSLamb () ())) =<<
#if MIN_VERSION_aeson(2,1,2)
    (throwDecode :: BSL.ByteString -> IO (FreeFunc Prims () ())) bs)
#else
    (fromJust . decode :: BSL.ByteString -> IO (FreeFunc Prims () ())) bs)
#endif
