{-# LANGUAGE Safe #-}

module Control.Category.Primitive.Extra (PrimitiveExtra(..)) where

import Control.Arrow (Kleisli (..))
import Data.Text qualified as T
import Data.Text (Text)

class PrimitiveExtra cat where
    intToString :: cat Int Text
    concatString :: cat (Text, Text) Text
    constString :: Text → cat a Text

instance PrimitiveExtra (->) where
    intToString = T.show
    concatString = uncurry (<>)
    constString = const

instance Monad m ⇒ PrimitiveExtra (Kleisli m) where
    intToString = Kleisli $ pure . T.show
    concatString = Kleisli $ pure . uncurry (<>)
    constString = Kleisli . const . pure

