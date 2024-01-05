{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wwarn -Wno-unsafe #-}

module Main (main) where

import Control.Category
import Control.Category.Interpret
import Data.ByteString.Lazy.Char8  qualified as BSL
import Data.Code.Haskell
import Data.Function.Free.Abstract
import Data.Prims
import Data.Render.File.WithShorthand
import Data.Yaml                   qualified as Y
import Prelude                     hiding (id, (.))
import System.Executable

-- | Compiles a category from YAML category file to a Haskell function source file.
main ∷ IO ()
main = readToWrite (\bs ->
    (pure . renderFileWithShorthand :: HS () () → IO BSL.ByteString) =<<
    (pure . interpret :: FreeFunc Prims () () → IO (HS () ())) =<<
    (Y.decodeThrow . BSL.toStrict :: BSL.ByteString → IO (FreeFunc Prims () ())) bs)
