{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wwarn -Wno-unsafe #-}

module Main (main) where

import Control.Arrow               (Kleisli (..))
import Control.Category
import Control.Category.Interpret
import Data.ByteString.Lazy.Char8  qualified as BSL
import Data.Function.Free.Abstract
import Data.Primitive.Prims
import Data.Yaml                   qualified as Y
import Prelude                     hiding (id, (.))
import System.Executable

-- | Compiles a category from YAML category file to a Haskell function source file.
main ∷ IO ()
main = readToOp (\bs ->
        flip runKleisli () =<<
        (pure . interpret :: FreeFunc Prims () () → IO (Kleisli IO () ())) =<<
        (Y.decodeThrow . BSL.toStrict :: BSL.ByteString → IO (FreeFunc Prims () ())) bs)
