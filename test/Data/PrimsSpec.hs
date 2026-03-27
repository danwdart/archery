module Data.PrimsSpec (spec) where

import Control.Category.Interpret
import Control.Category.Primitive.Bool
import Control.Category.Primitive.String
import Data.Aeson
import Data.Char
import Data.Text                         qualified as T
import Data.Maybe
import Data.Prims
import Data.Text.Arbitrary               (Text)
import Test.Hspec                        hiding (runIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Function.Free.Abstract

{- HLINT ignore "Use camelCase" -}

-- @TODO random functions
-- @TODO proper name of this equality function - isomorphic?

prop_ReverseStringIsCorrectViaEncodeDecodeInterpret ∷ Text → Property
prop_ReverseStringIsCorrectViaEncodeDecodeInterpret t = T.length t > 1 && T.all (\c -> notElem c "$" && isPrint c && isAscii c) t ==> withNumTests 200 $
    interpret ((fromJust (decode (encode (ReverseString :: Prims Text Text)))) :: Prims Text Text) t === reverseString t

prop_EqualIsCorrectViaEncodeDecodeInterpret ∷ (Int, Int) → Property
prop_EqualIsCorrectViaEncodeDecodeInterpret is = withNumTests 200 $
    interpret (fromJust (decode (encode (Equal :: Prims (Int, Int) Bool))) :: Prims (Int, Int) Bool) is === eq is

spec ∷ Spec
spec = parallel . describe "Data.Prims" $ do
    -- needs a better gen
    xdescribe "execution isomorphism" $ do
        prop "reverseString is correct" prop_ReverseStringIsCorrectViaEncodeDecodeInterpret
        prop "equal is correct" prop_EqualIsCorrectViaEncodeDecodeInterpret
    describe "JSON isomorphism" $ do
        it "ReverseString is isomorphic to its JSON representation" $
            decode (encode (ReverseString)) `shouldBe` Just ReverseString
        it "Equal is isomorphic to its JSON representation" $
            decode (encode (Equal :: Prims (Int, Int) Bool)) `shouldBe` Just (Equal :: Prims (Int, Int) Bool)
