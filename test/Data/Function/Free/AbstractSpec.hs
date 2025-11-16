module Data.Function.Free.AbstractSpec (spec) where

import Control.Category.Interpret
import Data.Aeson
import Data.Function.Free.Abstract
import Data.Prims
import Test.Hspec                  hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec ∷ Spec
spec = parallel $ pure ()
