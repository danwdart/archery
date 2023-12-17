{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

-- | Program module. Like Func, but dynamically imports modules as required.
module Data.Code.Haskell.Func where

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
import Control.Exception                  hiding (bracket)
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8         qualified as BSL
import Data.Map                           (Map)
import Data.Map                           qualified as M
import Data.Render.File
import Data.Render.Statement
import Data.Set                           (Set)
import Data.Set                           qualified as S
import Data.String
import Data.Typeable
import GHC.IO.Exception
import Prelude                            hiding (id, (.))
import System.Process
import Text.Read

type Module = BSL.ByteString

type ImportedFunctionName = BSL.ByteString

type RenderedStatement = BSL.ByteString

data HSFunc a b = HSFunc {
    imports :: Map Module (Set ImportedFunctionName),
    code    :: RenderedStatement
} deriving (Eq, Show)

toCLIImports ∷ HSFunc a b → [String]
toCLIImports HSFunc { imports = is } = M.toList is >>= \(moduleName, _ {-imports'-}) -> ["-e", ":l", BSL.unpack moduleName, "-e", BSL.unpack $ "import " <> moduleName {-}<> " (" <> BSL.intercalate ", " (S.toList imports') <> ")"-}]

toFileImports ∷ HSFunc a b → [BSL.ByteString]
toFileImports HSFunc { imports = is } = (\(moduleName, imports') -> "import " <> moduleName <> " (" <> BSL.intercalate ", " (S.toList imports') <> ")") <$> M.toList is

instance IsString (HSFunc a b) where
    fromString = HSFunc [] . BSL.pack

instance RenderStatement (HSFunc a b) where
    renderStatement HSFunc { code = s } = s -- @TODO is

-- TODO name this
instance (Typeable a, Typeable b) ⇒ RenderFile (HSFunc a b) where
    renderFile cat = "module Func where\n\n" <>
        BSL.unlines (toFileImports cat) <>
        "\nfunc :: " <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <> "\nfunc = " <>
        renderStatement cat

instance Bracket HSFunc where
    bracket s = HSFunc (imports s) $ "(" <> renderStatement s <> ")"

instance Category HSFunc where
    id = HSFunc [("Control.Category", ["id"])] "id"
    a . b = HSFunc (imports a <> imports b <> [("Control.Category", ["(.)"])]) $ "(" <> renderStatement a <> " . " <> renderStatement b <> ")"

instance Cartesian HSFunc where
    copy = HSFunc [("Control.Category.Cartesian", ["copy"])] "copy"
    consume = HSFunc [("Control.Category.Cartesian", ["consume"])] "consume"
    fst' = "fst"
    snd' = "snd"

instance Cocartesian HSFunc where
    injectL = "Left"
    injectR = "Right"
    unify = HSFunc [("Control.Category.Cocartesian", ["unify"])] "unify"
    tag = HSFunc [("Control.Category.Cartesian", ["tag"])] "tag"

instance Strong HSFunc where
    -- @TODO apply? Bracket?
    first' f = HSFunc ([("Data.Bifunctor", ["first"])] <> imports f) $ "(first " <> renderStatement f <> ")"
    second' f = HSFunc ([("Data.Bifunctor", ["second"])] <> imports f) $ "(second " <> renderStatement f <> ")"

instance Choice HSFunc where
    left' f = HSFunc (imports f) $ "(\\case { Left a -> Left (" <> renderStatement f <> " a); Right a -> Right a; })"
    right' f = HSFunc (imports f) $ "(\\case { Left a -> Left a; Right a -> Right (" <> renderStatement f <> " a); })"

instance Symmetric HSFunc where
    swap = "(\\(a, b) -> (b, a))"
    swapEither = "(\\case { Left a -> Right a; Right a -> Left a; })"
    reassoc = "(\\(a, (b, c)) -> ((a, b), c))"
    reassocEither = "(\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c })"

-- instance Cochoice HSFunc where

-- instance Costrong HSFunc where

-- instance Apply HSFunc where

instance PrimitiveBool HSFunc where
    eq = HSFunc [("Control.Category", ["(.)"]), ("Control.Arrow", ["arr"])] "(arr . uncurry $ (==))"

instance PrimitiveConsole HSFunc where
    outputString = HSFunc [("Control.Arrow", ["Kleisli(..)"])] "(Kleisli putStr)"
    inputString = HSFunc [("Control.Arrow", ["Kleisli(..)"])] "(Kleisli (const getContents))"

instance PrimitiveExtra HSFunc where
    intToString = "show"
    concatString = "(uncurry (<>))"
    constString s = fromString $ "(const \"" <> s <> "\")"

instance PrimitiveFile HSFunc where
    readFile' = HSFunc [("Control.Arrow", ["Kleisli(..)"])] "(Kleisli $ liftIO . readFile)"
    writeFile' = HSFunc [("Control.Arrow", ["Kleisli(..)"])] "(Kleisli $ liftIO . uncurry writeFile)"

instance PrimitiveString HSFunc where
    reverseString = HSFunc [("Control.Arrow", ["arr"])] "(arr reverse)"

instance Numeric HSFunc where
    num n = fromString $ "(const " <> show n <> ")"
    negate' = "negate"
    add = "(uncurry (+))"
    mult = "(uncurry (*))"
    div' = "(uncurry div)"
    mod' = "(uncurry mod)"

-- I don't quite know how to call ghci or cabal repl to include the correct functions here, so the tests are skipped.

-- @TODO escape shell - Text.ShellEscape?
instance ExecuteHaskell HSFunc where
    executeViaGHCi cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XLambdaCase",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toCLIImports cat <>
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
instance ExecuteStdio HSFunc where
    executeViaStdio cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XLambdaCase",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toCLIImports cat <>
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
