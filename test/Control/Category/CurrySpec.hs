module Control.Category.CurrySpec (spec) where

import Control.Category.Curry
import Test.Hspec

spec ∷ Spec
spec = parallel . xdescribe "todo" $ pure ()
