{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wwarn -Wno-unsafe -Wno-unused-imports #-}

module Main (main) where

import Control.Category
import Control.Category.Interpret
import Data.Aeson
import Data.Aeson.Compat
import Data.ByteString.Lazy.Char8  qualified as BSL
import Data.Code.Haskell
import Data.Function.Free.Abstract
import Data.Prims
import Data.Render.Statement.WithShorthand
import Prelude                     hiding (id, (.))
import System.Executable

-- | Compiles a category from YAML category file to a Haskell function source file.
main ∷ IO ()
main = readToWrite (\bs ->
    (pure . renderStatementWithShorthand :: HS () () → IO BSL.ByteString) =<<
    (pure . interpret :: FreeFunc Prims () () → IO (HS () ())) =<<
    (throwDecode :: BSL.ByteString → IO (FreeFunc Prims () ())) bs)
