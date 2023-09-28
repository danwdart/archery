module Control.Category.Primitive.FileSpec where

import Control.Arrow                   (Kleisli (..))
import Control.Category.Primitive.File
import System.Directory
import Test.Hspec

spec ∷ Spec
spec = describe "File" .
    describe "Kleisli" .
        it "writes a file and verifies it" $ do
            runKleisli writeFile' ("/tmp/a-file", "sample-contents")
            runKleisli readFile' "/tmp/a-file" `shouldReturn` "sample-contents"
            removeFile "/tmp/a-file"
