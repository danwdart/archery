{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wwarn -Wno-unsafe -Wno-unused-imports #-}

module Main (main) where

import Control.Arrow (Kleisli(..))
import Control.Category
import Control.Category.Interpret
import Data.Aeson
import Data.Aeson.Compat
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Function.Free.Abstract
import Data.Prims
import System.Executable
import Prelude hiding ((.), id)

-- | Compiles a category from YAML category file to a Haskell function source file.
main ∷ IO ()
main = readToOp (\bs ->
    flip runKleisli () =<<
    (pure . interpret :: FreeFunc Prims () () -> IO (Kleisli IO () ())) =<<
    (throwDecode :: BSL.ByteString -> IO (FreeFunc Prims () ())) bs)
