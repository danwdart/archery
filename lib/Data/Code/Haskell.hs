{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances              #-}
{-# LANGUAGE Unsafe              #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

-- | Program module. Like Func, but dynamically imports modules as required.
module Data.Code.Haskell where

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
import Data.Maybe
import Data.Render.File.WithImports
import Data.Render.File.WithDefinitions
import Data.Render.Statement
import Data.Set                           (Set)
import Data.Set                           qualified as S
import Data.String
import Data.Typeable
import GHC.IO.Exception
import GHC.IsList
import Prelude                            hiding (id, (.))
import System.Process
import Text.Read

type Module = BSL.ByteString
type FunctionName = BSL.ByteString
type RenderedStatement = BSL.ByteString
type Shorthand = BSL.ByteString
type Definition = BSL.ByteString

newtype Imports = Imports {
    unImports :: Map Module (Set (FunctionName, Maybe Definition))
}
    deriving stock (Eq, Show)
    deriving newtype IsList

instance Semigroup Imports where
    Imports a <> Imports b = Imports $ M.unionWith (<>) a b

deriving instance Monoid Imports

data HS a b = HS {
    imports :: Imports,
    export :: Maybe (Module, FunctionName, Definition),
    shorthand :: Shorthand
} deriving (Eq, Show)

toCLIImports ∷ HS a b → [String]
toCLIImports HS { imports = Imports is } = M.toList is >>= \(moduleName, _ {-imports'-}) -> [{-}"-e", ":l", BSL.unpack moduleName, -} "-e", BSL.unpack $ "import " <> moduleName {-}<> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")"-}]

toFileImports ∷ HS a b → [BSL.ByteString]
toFileImports HS { imports = Imports is } = (\(moduleName, imports') -> "import " <> moduleName <> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")") <$> M.toList is

filterExternal :: Imports -> Imports
filterExternal = Imports . M.filter (not . null) . fmap (S.filter (isNothing . snd)) . unImports

toExternalFileImports ∷ HS a b → [BSL.ByteString]
toExternalFileImports HS { imports = is } = (\(moduleName, imports') -> "import " <> moduleName <> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")") <$> M.toList (unImports $ filterExternal is)

exportToImports :: Maybe (Module, FunctionName, Definition) -> Imports
exportToImports = maybe mempty (\(module', functionName', definition') -> Imports [(module', [(functionName', Just definition')])])

renderDefinitions :: HS a b -> [BSL.ByteString]
renderDefinitions HS { imports = Imports is } = M.toList is >>= (fmap (\(fnName', mDefinition) -> case mDefinition of
    Nothing -> ""
    Just definition' -> "\n" <> fnName' <> " = " <> definition' -- TODO types
    ) . S.toList . snd)

instance IsString (HS a b) where
    fromString = HS [] Nothing . BSL.pack

instance RenderStatement (HS a b) where
    renderStatement HS { shorthand = s } = s -- @TODO is

-- TODO name this
instance (Typeable a, Typeable b) ⇒ RenderFileWithDefinitions (HS a b) where
    renderFileWithDefinitions cat@HS { export = export' } =
        "{-# LANGUAGE LambdaCase #-}" <>
        "\nmodule " <> module' <> " where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (renderDefinitions cat) <>
        "\n" <> functionName' <> " :: " <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> functionName' <> " = " <> renderStatement cat where
            (module', functionName', _definition') = fromMaybe ("Main", "main", "") export'

instance (Typeable a, Typeable b) ⇒ RenderFileWithImports (HS a b) where
    renderFileWithImports cat@HS { export = export' } =
        "{-# LANGUAGE LambdaCase #-}" <>
        "\nmodule " <> module' <> " where\n\n" <>
        BSL.unlines (toFileImports cat) <>
        "\n" <> functionName' <> " :: " <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> functionName' <> " = " <> renderStatement cat where
            (module', functionName', _definition') = fromMaybe ("Main", "main", "") export'

instance Bracket HS where
    bracket s = s { shorthand = "(" <> renderStatement s <> ")" }

instance Category HS where
    id = "id"
    a . b = HS (imports a <> imports b <> exportToImports (export a) <> exportToImports (export b)) Nothing $ "(" <> renderStatement a <> " . " <> renderStatement b <> ")"

instance Cartesian HS where
    copy = HS [] (Just ("Control.Category.Cartesian", "copy", "\\x -> (x, x)")) "copy"
    consume = HS [] (Just ("Control.Category.Cartesian", "consume", "\\x -> ()")) "consume"
    fst' = "fst"
    snd' = "snd"

instance Cocartesian HS where
    injectL = "Left"
    injectR = "Right"
    unify = HS [] (Just ("Control.Category.Cocartesian", "unify", "\\case { Left a -> a; Right a -> a; }")) "unify"
    tag = HS [] (Just ("Control.Category.Cartesian", "tag", "\\case { (False, a) -> Left a; (True, a) -> Right a; }")) "tag"

instance Strong HS where
    -- @TODO apply? Bracket?
    first' f = HS ([("Data.Bifunctor", [("first", Nothing)])] <> imports f) Nothing $ "(first " <> renderStatement f <> ")"
    second' f = HS ([("Data.Bifunctor", [("second", Nothing)])] <> imports f) Nothing $ "(second " <> renderStatement f <> ")"

instance Choice HS where
    left' f = HS (imports f) Nothing $ "(\\case { Left a -> Left (" <> renderStatement f <> " a); Right a -> Right a; })"
    right' f = HS (imports f) Nothing $ "(\\case { Left a -> Left a; Right a -> Right (" <> renderStatement f <> " a); })"

instance Symmetric HS where
    swap = HS [] (Just ("Control.Category.Symmetric", "swap", "\\(a, b) -> (b, a)")) "swap"
    swapEither = HS [] (Just ("Control.Category.Symmetric", "swapEither", "(\\case { Left a -> Right a; Right a -> Left a; })")) "swapEither"
    reassoc = HS [] (Just ("Control.Category.Symmetric", "reassoc", "(\\(a, (b, c)) -> ((a, b), c))")) "reassoc"
    reassocEither = HS [] (Just ("Control.Category.Symmetric", "reassocEither", "(\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c })")) "reassocEither"

-- instance Cochoice HS where

-- instance Costrong HS where

-- instance Apply HS where

instance PrimitiveBool HS where
    eq = HS [("Control.Arrow", [("arr", Nothing)])] (Just ("Control.Category.Primitive.Bool", "eq", "(arr . uncurry $ (==))")) "eq"

instance PrimitiveConsole HS where
    outputString = HS [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just ("Control.Category.Primitive.Console", "outputString", "(Kleisli putStr)")) "outputString"
    inputString = HS [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just ("Control.Category.Primitive.Console", "inputString", "(Kleisli (const getContents))")) "inputString"

instance PrimitiveExtra HS where
    intToString = "show"
    concatString = HS [] (Just ("Control.Category.Primitive.Extra", "concatString", "(uncurry (<>))")) "concatString"
    constString s = fromString $ "(const \"" <> s <> "\")"

instance PrimitiveFile HS where
    readFile' = HS [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just ("Control.Category.Primitive.File", "readFile'", "(Kleisli $ liftIO . readFile)")) "readFile'"
    writeFile' = HS [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just ("Control.Category.Primitive.File", "writeFile'", "(Kleisli $ liftIO . uncurry writeFile)")) "writeFile'"

instance PrimitiveString HS where
    reverseString = HS [("Control.Arrow", [("arr", Nothing)])] (Just ("Control.Category.Primitive.String", "reverseString", "(arr reverse)")) "reverseString"

instance Numeric HS where
    num n = fromString $ "(const " <> show n <> ")"
    negate' = "negate"
    add = HS [] (Just ("Control.Category.Numeric", "add", "(uncurry (+))")) "add"
    mult = HS [] (Just ("Control.Category.Numeric", "mult", "(uncurry (*))")) "mult"
    div' = HS [] (Just ("Control.Category.Numeric", "div'", "(uncurry div)")) "div'"
    mod' = HS [] (Just ("Control.Category.Numeric", "mod'", "(uncurry mod)")) "mod'"

-- I don't quite know how to call ghci or cabal repl to include the correct functions here, so the tests are skipped.

-- @TODO escape shell - Text.ShellEscape?
instance ExecuteHaskell HS where
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
instance ExecuteStdio HS where
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
