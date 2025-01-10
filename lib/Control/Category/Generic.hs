{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module Control.Category.Generic (Generic(..)) where

import Control.Category
import Control.Category.Apply
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.Haskell.Longhand
import Control.Category.Execute.Haskell.Imports
import Control.Category.Execute.Stdio.Longhand
import Control.Category.Execute.Stdio.Imports
import Control.Category.Numeric
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Curried
import Control.Category.Primitive.Extra
import Control.Category.Primitive.File
import Control.Category.Primitive.String
import Control.Category.Strong
import Control.Category.Symmetric
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8                       qualified as BSL
import Data.Kind
import Data.Render.Statement.Longhand
import Data.Render.Statement.Shorthand
import Data.String
-- import Data.Tuple.Triple
import Prelude                                          hiding (id, (.))
import System.Process
import Text.Read

class Generic cat

instance (Generic cat, IsString cat, Apply cat) ⇒ Category cat where
    id = "id"
    a . b = apply (apply ("compose" a) b)

instance (Generic cat, IsString cat) ⇒ Cartesian cat where
    copy = "copy"
    consume = "consume"
    fst' = "fst"
    snd' = "snd"

instance (Generic cat, IsString cat) ⇒ Cocartesian cat where
    injectL = "injectL"
    injectR = "injectR"
    unify = "unify"
    tag = "tag"

instance (Generic cat, IsString cat, Apply cat) ⇒ Strong cat where
    first' f = cat $ apply "first" f
    second' f = cat $ apply "second" f

instance (Generic cat, IsString cat, Apply cat) ⇒ Choice cat where
    left' f = cat $ apply "left" f
    right' f = cat $ apply "right" f

instance (Generic cat, IsString cat) ⇒ Symmetric cat where
    swap = "swap"
    swapEither = "swapEither"
    reassoc = "reassoc"
    reassocEither = "reassocEither"

-- instance Cochoice cat where

-- instance Costrong cat where

-- instance Apply cat where

instance (Generic cat, IsString cat) ⇒ PrimitiveBool cat where
    eq = "eq"

instance (Generic cat, IsString cat) ⇒ PrimitiveString cat where
    reverseString = "reverseString"

instance (Generic cat, IsString cat) ⇒ PrimitiveConsole cat where
    outputString = "outputString"
    inputString = "inputString"

instance (Generic cat, IsString cat, Apply cat) ⇒ Numeric cat where
    num n = apply "num" (BSL.pack (show n))
    negate' = "negate"
    add = "add"
    mult = "mult"
    div' = "div"
    mod' = "mod"
