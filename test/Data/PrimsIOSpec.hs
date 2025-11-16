module Data.PrimsIOSpec (spec) where

import Control.Category.Interpret
import Control.Category.Primitive.Bool
import Control.Category.Primitive.String
import Data.Aeson
import Data.Char
import Data.Text                         qualified as T
import Data.Maybe
import Data.PrimsIO
import Data.Text.Arbitrary               (Text)
import Test.Hspec                        hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Function.Free.Abstract
import Control.Arrow

{- HLINT ignore "Use camelCase" -}

-- @TODO random functions
-- @TODO proper name of this equality function - isomorphic?

-- runkleisli

prop_ReverseStringIsCorrectViaEncodeDecodeInterpret ∷ Text → Property
prop_ReverseStringIsCorrectViaEncodeDecodeInterpret t = T.length t > 1 && T.all (\c -> notElem c "$" && isPrint c && isAscii c) t ==> withMaxSuccess 200 $ do
    expected <- runKleisli reverseString t
    let Just fn = decode (encode (ReverseString :: PrimsIO Text Text)) :: Maybe (PrimsIO Text Text)
    result <- runKleisli (interpret fn) t
    result `shouldBe` expected

prop_EqualIsCorrectViaEncodeDecodeInterpret ∷ (Int, Int) → Property
prop_EqualIsCorrectViaEncodeDecodeInterpret is = withMaxSuccess 200 $ do
    expected <- runKleisli eq is
    let Just fn = decode (encode (Equal :: PrimsIO (Int, Int) Bool)) :: Maybe (PrimsIO (Int, Int) Bool)
    result <- runKleisli (interpret fn) is
    result `shouldBe` expected

spec ∷ Spec
spec = parallel . describe "Data.PrimsIO" $ do
    -- needs a better gen
    xdescribe "execution isomorphism" $ do
        prop "reverseString is correct" prop_ReverseStringIsCorrectViaEncodeDecodeInterpret
        prop "equal is correct" prop_EqualIsCorrectViaEncodeDecodeInterpret
    describe "JSON isomorphism" $ do
        it "ReverseString is isomorphic to its JSON representation" $
            decode (encode (ReverseString)) `shouldBe` Just ReverseString
        it "Equal is isomorphic to its JSON representation" $
            decode (encode (Equal :: PrimsIO (Int, Int) Bool)) `shouldBe` Just (Equal :: PrimsIO (Int, Int) Bool)
