{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module Data.Function.Arrowy where

import Control.Arrow
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Data.Text                          (Text)

arrowyIO ∷ (PrimitiveExtra cat, PrimitiveConsole cat, ArrowChoice cat) ⇒ cat () ()
arrowyIO = proc () -> do
    askName <- constString "Please enter your name." -< ()
    outputString -< askName
    name <- inputString -< ()
    askPw <- constString "And your password:" -< ()
    outputString -< askPw
    pwd <- inputString -< ()
    end <- constString "!" -< ()
    if (eq (pwd, "pwd123"))
    then do
        begin <- constString "Yeah: " -< ()
        middle <- concatString -< (begin, name)
        ret <- concatString -< (middle, end)
        outputString -< ret
    else do
        begin <- constString "Nah: " -< ()
        middle <- concatString -< (begin, name)
        ret <- concatString -< (middle, end)
        outputString -< ret


arrowy ∷ (ArrowChoice cat, PrimitiveExtra cat) ⇒ cat Text Text
arrowy = proc _pwd -> do
    end <- constString "!" -< ()
    -- if (eq (pwd, "pwd123"))
    -- then do
    --     begin <- constString "Yeah: " -< ()
    --     ret <- concatString -< (begin, end)
    --     returnA -< ret
    -- else do
    begin <- constString "Nah: " -< ()
    ret <- concatString -< (begin, end)
    returnA -< ret
-- applyDemo :: cat () Text
-- applyDemo = proc () -> dokoolsound
--     concatString (concatString (constString "Wonderful! You're in, ", constString "Bob"), constString "!") -< ()