{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Safe               #-}

module Data.Function.HelloWorld where

import Control.Category
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Prelude                          hiding (id, (.))

helloWorld :: (Category cat, PrimitiveConsole cat, PrimitiveExtra cat) => cat () ()
helloWorld = outputString . constString "Hello, World!\\n"