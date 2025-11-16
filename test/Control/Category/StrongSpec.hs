module Control.Category.StrongSpec (spec) where

import Control.Category.Strong
import Test.Hspec

spec ∷ Spec
spec = parallel . xdescribe "todo" $ pure ()
