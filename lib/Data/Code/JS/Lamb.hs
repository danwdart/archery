{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.Code.JS.Lamb where

import Control.Category
-- import Control.Category.Apply
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.JSON
import Control.Category.Execute.Stdio
import Control.Category.Numeric
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Control.Category.Primitive.File
import Control.Category.Primitive.String
import Control.Category.Strong
import Control.Category.Symmetric
import Control.Exception                  hiding (bracket)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy.Char8         qualified as BSL
import Data.Render.Statement
import Data.String
import GHC.IO.Exception
import Prelude                            hiding (id, (.))
import System.Process
import Text.Read

newtype JSLamb a b = JSLamb BSL.ByteString
    deriving (Eq, Show)

instance IsString (JSLamb a b) where
    fromString = JSLamb . BSL.pack

instance RenderStatement (JSLamb a b) where
    renderStatement (JSLamb f) = f

instance Bracket JSLamb where
    bracket s = JSLamb $ "(" <> renderStatement s <> ")"

instance Category JSLamb where
    id = "(x => x)"
    a . b = JSLamb $ "(x => " <> renderStatement a <> "(" <> renderStatement b <> "(x)))"

instance Cartesian JSLamb where
    copy = "(x => [x, x])"
    consume = "(x => null)"
    fst' = "(([x, _]) => x)"
    snd' = "(([_, y]) => y)"

instance Cocartesian JSLamb where
    injectL = "(x => ({ Left: x}))"
    injectR = "(x => ({ Right: x}))"
    unify = "(x => x.Left ? x.Left : x.Right)"
    tag = "(([b, x]) => b ? ({Right: x}) : ({Left: x}))"

instance Strong JSLamb where
    first' (JSLamb f) = JSLamb $ "(([x, y]) => [" <> f <> "(x), y])"
    second' (JSLamb f) = JSLamb $ "(([x, y]) => [x, " <> f <> "(y)])"

instance Choice JSLamb where
    left' (JSLamb f) = JSLamb $ "(x => x.Left ? ({ Left: " <> f <> " (x.Left) }) : x)"
    right' (JSLamb f) = JSLamb $ "(x => x.Right ? ({ Right: " <> f <> " (x.Right) }) : x)"

instance Symmetric JSLamb where
    swap = "(([a, b]) => ([b, a]))"
    swapEither = "(x => x.Left ? ({ Right: x.Left }) : ({ Left: x.Right }))"
    reassoc = "(([a, [b, c]]) => [[a, b], c])"
    reassocEither = error "Not yet implemented"

-- instance Cochoice JSLamb where

-- instance Costrong JSLamb where

-- instance Apply JSLamb where

instance PrimitiveBool JSLamb where
    eq = "(([x, y]) => x === y)"

instance PrimitiveConsole JSLamb where
    outputString = "console.log"
    inputString = "prompt"

instance PrimitiveExtra JSLamb where
    intToString = "(i => i.toString())"
    concatString = "(([a, b]) => a + b)"
    constString s = JSLamb $ "(() => \"" <> BSL.pack s <> "\")"

instance PrimitiveFile JSLamb where
    readFile' = "@TODO"
    writeFile' = "@TODO"

instance PrimitiveString JSLamb where
    reverseString = "(x => x.split('').reverse().join(''))"

instance Numeric JSLamb where
    num n = JSLamb $ "(_ => " <> BSL.pack (show n) <> ")"
    negate' = "(x => -x)"
    add = "(([x, y]) => x + y)"
    mult = "(([x, y]) => x * y)"
    div' = "(([x, y]) => x / y)"
    mod' = "(([x, y]) => x % y)"

-- @TODO escape shell - Text.ShellEscape?
instance ExecuteJSON JSLamb where
    executeViaJSON cat param = do
        let params ∷ [String]
            params = ["-e", "console.log(JSON.stringify(" <> BSL.unpack (renderStatement cat) <> "(" <> BSL.unpack (encode param) <> ")))"]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case eitherDecode (BSL.pack stdout) of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

instance ExecuteStdio JSLamb where
    executeViaStdio cat stdin = do
        let params ∷ [String]
            params = ["-e", BSL.unpack (renderStatement cat) <> "()"]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params (show stdin))
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case readEither stdout of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret
