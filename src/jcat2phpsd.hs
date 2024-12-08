{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wwarn -Wno-unsafe -Wno-unused-imports #-}

module Main (main) where

import Control.Category
import Control.Category.Interpret
import Data.Aeson
import Data.Aeson.Compat
import Data.ByteString.Lazy.Char8            qualified as BSL
import Data.Code.PHP
import Data.Function.Free.Abstract
import Data.Prims
import Data.Render.Statement.Longhand
import Prelude                               hiding (id, (.))
import System.Executable

-- | Compiles a category from YAML category file to a Haskell function source file.
main ∷ IO ()
main = readToWrite (\bs ->
    (pure . renderStatementLonghand :: PHP () () → IO BSL.ByteString) =<<
    (pure . interpret :: FreeFunc Prims () () → IO (PHP () ())) =<<
    (throwDecode :: BSL.ByteString → IO (FreeFunc Prims () ())) bs)
