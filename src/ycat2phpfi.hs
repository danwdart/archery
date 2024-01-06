{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wwarn -Wno-unsafe #-}

module Main (main) where

import Control.Category
import Control.Category.Interpret
import Data.ByteString.Lazy.Char8   qualified as BSL
import Data.Code.PHP
import Data.Function.Free.Abstract
import Data.Prims
import Data.Render.File.WithImports
import Data.Yaml                    qualified as Y
import Prelude                      hiding (id, (.))
import System.Executable

-- | Compiles a category from YAML category file to a Haskell function source file.
main ∷ IO ()
main = readToWrite (\bs ->
    (pure . renderFileWithImports :: PHP () () → IO BSL.ByteString) =<<
    (pure . interpret :: FreeFunc Prims () () → IO (PHP () ())) =<<
    (Y.decodeThrow . BSL.toStrict :: BSL.ByteString → IO (FreeFunc Prims () ())) bs)
