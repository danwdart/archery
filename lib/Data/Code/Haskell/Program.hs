{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

-- | Program module. Like Func, but dynamically imports modules as required.
module Data.Code.Haskell.Program where

import Control.Category
-- import Control.Category.Apply
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.Haskell
import Control.Category.Execute.Stdio
import Control.Category.Numeric
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Control.Category.Primitive.File
import Control.Category.Primitive.String
import Control.Category.Strong
import Control.Category.Symmetric
import Control.Exception                   hiding (bracket)
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8          qualified as BSL
import Data.Render.File
import Data.Render.Statement
import Data.Set                            (Set)
import Data.Set                            qualified as S
import Data.String
import Data.Typeable
import GHC.IO.Exception
import Prelude                             hiding (id, (.))
import System.Process
import Text.Read

type Import = BSL.ByteString

data HSProg a b = HSProg {
    imports :: Set Import,
    code    :: BSL.ByteString
} deriving (Eq, Show)

toImports ∷ HSProg a b → [String]
toImports (HSProg imports _) = S.toList imports >>= \importStr -> ["-e", "import " <> BSL.unpack importStr]

instance IsString (HSProg a b) where
    fromString = HSProg [] . BSL.pack

instance RenderStatement (HSProg a b) where
    renderStatement (HSProg _ f) = f -- @TODO is

instance (Typeable a, Typeable b) ⇒ RenderFile (HSProg a b) where
    renderFile cat = "module Func where\n\n" <>
        BSL.unlines (("import " <>) <$> S.toList (imports cat)) <>
        "\n\nfunc :: " <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <> "\nfunc = " <>
        renderStatement cat

instance Bracket HSProg where
    bracket s = HSProg (imports s) $ "(" <> renderStatement s <> ")"

instance Category HSProg where
    id = "id"
    a . b = HSProg (imports a <> imports b) $ "(" <> renderStatement a <> " . " <> renderStatement b <> ")"

instance Cartesian HSProg where
    copy = HSProg ["Control.Category.Cartesian"] "copy"
    consume = HSProg ["Control.Category.Cartesian"] "consume"
    fst' = "fst"
    snd' = "snd"

instance Cocartesian HSProg where
    injectL = "Left"
    injectR = "Right"
    unify = HSProg ["Control.Category.Cocartesian"] "unify"
    tag = HSProg ["Control.Category.Cartesian"] "tag"

instance Strong HSProg where
    -- @TODO apply? Bracket?
    first' f = HSProg (["Data.Bifunctor"] <> imports f) $ "(first " <> renderStatement f <> ")"
    second' f = HSProg (["Data.Bifunctor"] <> imports f) $ "(second " <> renderStatement f <> ")"

instance Choice HSProg where
    left' f = HSProg (imports f) $ "(\\case { Left a -> Left (" <> renderStatement f <> " a); Right a -> Right a; })"
    right' f = HSProg (imports f) $ "(\\case { Left a -> Left a; Right a -> Right (" <> renderStatement f <> " a); })"

instance Symmetric HSProg where
    swap = "(\\(a, b) -> (b, a))"
    swapEither = "(\\case { Left a -> Right a; Right a -> Left a; })"
    reassoc = "(\\(a, (b, c)) -> ((a, b), c))"
    reassocEither = "(\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c })"

-- instance Cochoice HSProg where

-- instance Costrong HSProg where

-- instance Apply HSProg where

instance PrimitiveBool HSProg where
    eq = "(arr . uncurry $ (==))"

instance PrimitiveConsole HSProg where
    outputString = "(Kleisli putStr)"
    inputString = "(Kleisli (const getContents))"

instance PrimitiveExtra HSProg where
    intToString = "show"
    concatString = "(uncurry (<>))"
    constString s = HSProg [] $ "(const \"" <> BSL.pack s <> "\")"

instance PrimitiveFile HSProg where
    readFile' = "(Kleisli $ liftIO . readFile)"
    writeFile' = "(Kleisli $ liftIO . uncurry writeFile)"

instance PrimitiveString HSProg where
    reverseString = "(arr reverse)"

instance Numeric HSProg where
    num n = HSProg [] $ "(const " <> fromString (show n) <> ")"
    negate' = "negate"
    add = "(uncurry (+))"
    mult = "(uncurry (*))"
    div' = "(uncurry div)"
    mod' = "(uncurry mod)"

-- I don't quite know how to call ghci or cabal repl to include the correct functions here, so the tests are skipped.

-- @TODO escape shell - Text.ShellEscape?
instance ExecuteHaskell HSProg where
    executeViaGHCi cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -XLambdaCase",
                "-e", "import Control.Arrow",
                "-e", "import Prelude hiding ((.), id)",
                "-e", "import Control.Category"
                ] <>
                toImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatement cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case readEither stdout of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

-- @TODO this passes too many arguments apparently...
-- This is because of the id and (.) using the (->) instance whereas I am running Kleisli below.
-- This means we need to deal with both within Haskell sessions. Let's try to use Pure/Monadic... or maybe HSPure / HSMonadic accepting only appropriate typeclasses / primitives?
instance ExecuteStdio HSProg where
    executeViaStdio cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":set -XLambdaCase",
                "-e", "import Control.Arrow",
                "-e", "import Prelude hiding ((.), id)",
                "-e", "import Control.Category"
                ] <>
                toImports cat <>
                [
                "-e", BSL.unpack (renderStatement cat) <> " ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case readEither stdout of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

-- @ TODO JSON
