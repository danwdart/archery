{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Control.Category.Primitive.FileSpec (spec) where

import Control.Arrow                   (Kleisli (..))
import Control.Category.Primitive.File
import System.Directory
import System.OsPath                   (unsafeEncodeUtf)
import Test.Hspec

spec ∷ Spec
spec = parallel . describe "File" .
    describe "Kleisli" .
        it "writes a file and verifies it" $ do
            runKleisli writeFile' (unsafeEncodeUtf "/tmp/a-file", "sample-contents")
            runKleisli readFile' (unsafeEncodeUtf "/tmp/a-file") `shouldReturn` "sample-contents"
            -- base cleanup
            removeFile "/tmp/a-file"
