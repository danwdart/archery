{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main (main) where

import Control.Category
-- import Control.Category.Interpret
-- import Data.Aeson                  qualified as A
-- import Data.ByteString.Char8      qualified as BS
import Data.ByteString.Lazy.Char8  qualified as BSL
-- import Data.Code.Haskell.Func
import Data.Code.Haskell.Lamb
import Data.Code.JS.Lamb
-- import Data.Code.JS.Func
import Data.Code.PHP.Lamb
-- import Data.Person
-- import Data.Prims
-- import Data.Render.File
import Data.Render.Statement
-- import Data.Person
-- import Data.Yaml                   qualified as Y
import Prelude hiding ((.), id)
import System.Directory

-- For each language...
-- For each type...

main :: IO ()
main = do
    -- removeDirectoryRecursive "library"

    createDirectoryIfMissing True "library/Control"

    BSL.writeFile "library/Control/Category.hs" $
        "module Control.Category where\n\n" <>
        "id = " <> renderStatement (id :: HSLamb a a) <> "\n"

    createDirectoryIfMissing True "jsbits/control"

    BSL.writeFile "jsbits/control/category.js" $
        "export const id = " <> renderStatement (id :: JSLamb a a) <> ";\n"

    createDirectoryIfMissing True "phpbits/Control"

    BSL.writeFile "phpbits/Control/Category.php" $
        "<?php\n" <>
        "$id = " <> renderStatement (id :: PHPLamb a a) <> ";\n"