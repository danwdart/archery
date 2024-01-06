{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Data.Code.PHP where

import Control.Category
-- import Control.Category.Apply
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.JSON.WithDefinitions
import Control.Category.Execute.JSON.WithImports
import Control.Category.Execute.Stdio.WithDefinitions
import Control.Category.Execute.Stdio.WithImports
import Control.Category.Numeric
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Control.Category.Primitive.File
import Control.Category.Primitive.String
import Control.Category.Strong
import Control.Category.Symmetric
import Control.Exception                              hiding (bracket)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy.Char8                     qualified as BSL
-- import Data.Map (Map)
-- import Data.Map qualified as M
import Data.Render.File.WithImports
import Data.Render.Statement
import Data.String
import GHC.IO.Exception
import Prelude                                        hiding (id, (.))
import System.Process
import Text.Read

newtype PHP a b = PHP BSL.ByteString
    deriving (Eq, Show)

instance IsString (PHP a b) where
    fromString = PHP . BSL.pack

instance RenderStatement (PHP a b) where
    renderStatement (PHP f) = f

instance RenderFileWithImports (PHP a b) where
    renderFileWithImports p =
        "<?php\n" <>
        "require(\"vendor/autoload.php\");\n" <>
        "\nreturn " <>
        renderStatement p <>
        ";\n"

{-}
instance RenderFileWithImports (Map BSL.ByteString (PHP a b)) where
    renderFileWithImports p =
        "<?php\n" <>
        "require(\"vendor/autoload.php\");\n" <>
        "\n/* TODO */"
-}

instance Bracket PHP where
    bracket s = PHP $ "(" <> renderStatement s <> ")"

instance Category PHP where
    id = "(fn ($x) => $x)"
    PHP a . PHP b = PHP $ "(fn ($x) => " <> a <> "(" <> b <> "($x)))"

instance Cartesian PHP where
    copy = "(fn ($x) => [$x, $x])"
    consume = "(fn ($x) => null)"
    fst' = "(fn ($x) => $x[0])"
    snd' = "(fn ($x) => $x[1])"

instance Cocartesian PHP where
    injectL = "(fn ($x) => ['tag' => 'left', 'value' => $x])"
    injectR = "(fn ($x) => ['tag' => 'right', 'value' => $x])"
    unify = "(fn ($x) => $x['value'])"
    tag = "(fn ($x) => ['tag' => $x[0] ? 'right' : 'left', 'value' => $x[1]])"

instance Strong PHP where
    first' (PHP f) = PHP $ "(fn ($x) => [" <> f <> "($x[0]), $x[1]])"
    second' (PHP f) = PHP $ "(fn ($x) => [$x[0], " <> f <> "($x[1])])"

instance Choice PHP where
    left' (PHP f) = PHP $ "(fn ($x) => ['tag' => $x['tag'], 'value' => $x['tag'] === 'left' ? " <> f <> " ($x['value']) : $x['value']])"
    right' (PHP f) = PHP $ "(fn ($x) => ['tag' => $x['tag'], 'value' => $x['tag'] === 'right' ? " <> f <> " ($x['value']) : $x['value']])"

instance Symmetric PHP where
    swap = "(fn ($a) => [$a[1], $a[0]]))"
    swapEither = "(fn ($a) => (['tag' => $a['tag'] === \"left\" ? \"right\" : \"left\", 'value' => $a['value']]))"
    reassoc = "(([a, [b, c]]) => [[a, b], c])"
    reassocEither = error "Not yet implemented"

-- instance Cochoice PHP where

-- instance Costrong PHP where

-- instance Apply PHP where

instance PrimitiveBool PHP where
    eq = "(fn ($x) => $x[0] === $x[1])"

instance PrimitiveConsole PHP where
    outputString = "print"
    inputString = "(function () { $r = fopen('php://stdin', 'r'); $ret = fgets($r); fclose($r); return $ret; })" -- difficult to not miss nullary functions

instance PrimitiveExtra PHP where
    intToString = "(fn ($i) => strval($i))"
    concatString = "(fn ([$a, $b]) => $a . $b)"
    constString s = PHP $ "(fn () => \"" <> BSL.pack s <> "\")"

instance PrimitiveFile PHP where
    readFile' = "@TODO"
    writeFile' = "@TODO"

instance PrimitiveString PHP where
    reverseString = "(fn ($x) => strrev($x))"

instance Numeric PHP where
    num n = PHP $ "(fn () => " <> BSL.pack (show n) <> ")"
    negate' = "(fn ($x) => -$x)"
    add = "(fn ($x) => $x[0] + $x[1])"
    mult = "(fn ($x) => $x[0] * $x[1])"
    div' = "(fn ($x) => $x[0] / $x[1])"
    mod' = "(fn ($x) => $x[0] % $x[1])"

-- @TODO escape shell - Text.ShellEscape?
instance ExecuteJSONWithDefinitions PHP where
    executeViaJSONWithDefinitions cat param = do
        let params ∷ [String]
            params = ["-r", "\"print(json_encode((" <> BSL.unpack (renderStatement cat) <> ")(" <> BSL.unpack (encode param) <> ")));\""]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "php" params [])
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run php with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case eitherDecode (BSL.pack stdout) of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

instance ExecuteJSONWithImports PHP where
    executeViaJSONWithImports cat param = do
        let params ∷ [String]
            params = ["-r", "\"print(json_encode((" <> BSL.unpack (renderStatement cat) <> ")(" <> BSL.unpack (encode param) <> ")));\""]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "php" params [])
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run php with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case eitherDecode (BSL.pack stdout) of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

instance ExecuteStdioWithDefinitions PHP where
    -- @TODO figure out why we have to have something here for argument - for now using null...
    executeViaStdioWithDefinitions cat stdin = do
        let params ∷ [String]
            params = ["-r", "(" <> BSL.unpack (renderStatement cat) <> ")(null);"]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "php" params (show stdin))
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run php with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case readEither stdout of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

instance ExecuteStdioWithImports PHP where
    -- @TODO figure out why we have to have something here for argument - for now using null...
    executeViaStdioWithImports cat stdin = do
        let params ∷ [String]
            params = ["-r", "(" <> BSL.unpack (renderStatement cat) <> ")(null);"]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "php" params (show stdin))
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run php with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case readEither stdout of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret
