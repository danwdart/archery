{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE Unsafe                     #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

-- | Program module. Like Func, but dynamically imports modules as required.
module Data.Code.Haskell where

import Control.Category
-- import Control.Category.Apply
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.Haskell.WithDefinitions
import Control.Category.Execute.Haskell.WithImports
import Control.Category.Execute.Haskell.WithShorthand
import Control.Category.Execute.Stdio.WithDefinitions
import Control.Category.Execute.Stdio.WithImports
import Control.Category.Execute.Stdio.WithShorthand
import Control.Category.Numeric
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Control.Category.Primitive.File
import Control.Category.Primitive.String
import Control.Category.Strong
import Control.Category.Symmetric
import Control.Exception                                hiding (bracket)
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8                       qualified as BSL
import Data.Code.Generic
-- import Data.Map                                         (Map)
import Data.Map                                         qualified as M
import Data.Maybe
import Data.Render.File.WithDefinitions
import Data.Render.File.WithImports
import Data.Render.File.WithShorthand
import Data.Render.Statement.WithDefinitions
import Data.Render.Statement.WithShorthand
-- import Data.Set                                         (Set)
import Data.Set                                         qualified as S
import Data.String
import Data.Typeable
import GHC.IO.Exception
-- import GHC.IsList
import Prelude                                          hiding (id, (.))
import System.Process
import Text.Read

newtype HS a b = HS {
    _code :: Code a b
} deriving stock (Eq, Show)

instance HasCode HS a b where
    code = _code

instance MkCode HS a b where
    mkCode i e s d = HS $ mkCode i e s d

toCLIImports ∷ HS a b → [String]
toCLIImports hs = M.toList (unImports . imports $ hs) >>= \(moduleName, _imports') -> [{-}"-e", ":l", BSL.unpack moduleName,-}"-e", BSL.unpack $ "import " <> moduleName {-<> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")\""-}]

toExternalCLIImports ∷ HS a b → [String]
toExternalCLIImports hs = M.toList (unImports $ filterExternal (imports hs)) >>= \(moduleName, _imports' ) -> [{-}"-e", ":l", BSL.unpack moduleName,-} "-e", BSL.unpack $ "import " <> moduleName {-<> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")\""-}]

toFileImports ∷ HS a b → [BSL.ByteString]
toFileImports hs = (\(moduleName, imports') -> "import " <> moduleName <> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")") <$> M.toList (unImports $ imports hs)

toExternalFileImports ∷ HS a b → [BSL.ByteString]
toExternalFileImports hs = (\(moduleName, imports') -> "import " <> moduleName <> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")") <$> M.toList (unImports $ filterExternal (imports hs))

renderDefinitions ∷ HS a b → [BSL.ByteString]
renderDefinitions hs = M.toList (unImports $ imports hs) >>= (fmap (\(fnName', mDefinition) -> case mDefinition of
    Nothing          -> ""
    Just definition' -> "\n" <> fnName' <> " = " <> definition' -- TODO types
    ) . S.toList . snd)

instance RenderStatementWithDefinitions (HS a b) where
    renderStatementWithDefinitions = definition -- @TODO is

instance RenderStatementWithShorthand (HS a b) where
    renderStatementWithShorthand = shorthand -- @TODO is

-- TODO name this
instance (Typeable a, Typeable b) ⇒ RenderFileWithShorthand (HS a b) where
    renderFileWithShorthand cat =
        "\nmodule " <> module' <> " where\n\n" <>
        BSL.unlines (renderDefinitions cat) <>
        "\n" <> functionName' <> " :: " <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> functionName' <> " = " <> renderStatementWithShorthand cat where
            Export {
                module' = module',
                functionName = functionName'
            } = fromMaybe (
                Export {
                    module' = "Main",
                    functionName = "main",
                    functionType = "IO ()"
                }
                ) (export cat)

instance (Typeable a, Typeable b) ⇒ RenderFileWithDefinitions (HS a b) where
    renderFileWithDefinitions cat =
        "\nmodule " <> module' <> " where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        "\n" <> functionName' <> " :: " <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> functionName' <> " = " <> renderStatementWithDefinitions cat where
            Export {
                module' = module',
                functionName = functionName'
            } = fromMaybe (
                Export {
                    module' = "Main",
                    functionName = "main",
                    functionType = "IO ()"
                }
                ) (export cat)

instance (Typeable a, Typeable b) ⇒ RenderFileWithImports (HS a b) where
    renderFileWithImports cat =
        "\nmodule " <> module' <> " where\n\n" <>
        BSL.unlines (toFileImports cat) <>
        "\n" <> functionName' <> " :: " <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> functionName' <> " = " <> renderStatementWithShorthand cat where
            Export {
                module' = module',
                functionName = functionName'
            } = fromMaybe (
                Export {
                    module' = "Main",
                    functionName = "main",
                    functionType = "IO ()"
                }
                ) (export cat)

instance Bracket HS where
    bracket hs@(HS s) = HS $ s {
        _shorthand = "(" <> renderStatementWithShorthand hs <> ")",
        _definition = "(" <> renderStatementWithDefinitions hs <> ")"
    }

instance Category HS where
    id = "id"
    a . b = HS $ Code {
        _imports = imports a <> imports b <> toImports a <> toImports b,
        _export = Nothing,
        _shorthand = "(" <> renderStatementWithShorthand a <> " . " <> renderStatementWithShorthand b <> ")",
        _definition = "(" <> renderStatementWithDefinitions a <> " . " <> renderStatementWithDefinitions b <> ")"
    }

instance Cartesian HS where
    copy = mkCode [] (Just (Export { module' = "Control.Category.Cartesian", functionName = "copy", functionType = "" })) "copy" "\\x -> (x, x)"
    consume = mkCode [] (Just (Export { module' = "Control.Category.Cartesian", functionName = "consume", functionType = "" })) "consume" "\\x -> ()"
    fst' = "fst"
    snd' = "snd"

instance Cocartesian HS where
    injectL = "Left"
    injectR = "Right"
    unify = mkCode [] (Just (Export { module' = "Control.Category.Cocartesian", functionName = "unify", functionType = "" })) "unify" "\\case { Left a -> a; Right a -> a; }"
    tag = mkCode [] (Just (Export { module' = "Control.Category.Cartesian", functionName = "tag", functionType = "" })) "tag" "\\case { (False, a) -> Left a; (True, a) -> Right a; }"

instance Strong HS where
    -- @TODO apply? Bracket?
    first' f = HS $ Code {
        _imports = [("Data.Bifunctor", [("first", Nothing)])] <> imports f,
        _export = Nothing,
        _shorthand = "(Data.Bifunctor.first (" <> renderStatementWithShorthand f <> "))",
        _definition = "(Data.Bifunctor.first (" <> renderStatementWithDefinitions f <> "))"
    }
    second' f = HS $ Code {
        _imports = [("Data.Bifunctor", [("second", Nothing)])] <> imports f,
        _export = Nothing,
        _shorthand = "(Data.Bifunctor.second (" <> renderStatementWithShorthand f <> "))",
        _definition = "(Data.Bifunctor.second (" <> renderStatementWithDefinitions f <> "))"
    }

instance Choice HS where
    left' f = HS $ Code {
        _imports = imports f,
        _export = Nothing,
        _shorthand = "(\\case { Left a -> Left ((" <> renderStatementWithShorthand f <> ") a); Right a -> Right a; })",
        _definition = "(\\case { Left a -> Left ((" <> renderStatementWithDefinitions f <> ") a); Right a -> Right a; })"
    }
    right' f = HS $ Code {
        _imports = imports f,
        _export = Nothing,
        _shorthand = "(\\case { Left a -> Left a; Right a -> Right ((" <> renderStatementWithShorthand f <> ") a); })",
        _definition = "(\\case { Left a -> Left a; Right a -> Right ((" <> renderStatementWithDefinitions f <> ") a); })"
    }

instance Symmetric HS where
    swap = mkCode [] (Just (Export { module' = "Control.Category.Symmetric", functionName = "swap", functionType = "" })) "swap" "\\(a, b) -> (b, a)"
    swapEither = mkCode [] (Just (Export { module' = "Control.Category.Symmetric", functionName = "swapEither", functionType = "" })) "swapEither" "(\\case { Left a -> Right a; Right a -> Left a; })"
    reassoc = mkCode [] (Just (Export { module' = "Control.Category.Symmetric", functionName = "reassoc", functionType = "" })) "reassoc" "(\\(a, (b, c)) -> ((a, b), c))"
    reassocEither = mkCode [] (Just (Export { module' = "Control.Category.Symmetric", functionName = "reassocEither", functionType = "" })) "reassocEither" "(\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c })"

-- instance Cochoice HS where

-- instance Costrong HS where

-- instance Apply HS where

instance PrimitiveBool HS where
    eq = mkCode [("Control.Arrow", [("arr", Nothing)])] (Just (Export { module' = "Control.Category.Primitive.Bool", functionName = "eq", functionType = "" })) "eq" "(arr . uncurry $ (==))"

instance PrimitiveConsole HS where
    outputString = mkCode [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just (Export { module' = "Control.Category.Primitive.Console", functionName = "outputString", functionType = "" })) "outputString" "(Kleisli putStr)"
    inputString = mkCode [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just (Export { module' = "Control.Category.Primitive.Console", functionName = "inputString", functionType = "" })) "inputString" "(Kleisli (const getContents))"

instance PrimitiveExtra HS where
    intToString = "show"
    concatString = mkCode [] (Just (Export { module' = "Control.Category.Primitive.Extra", functionName = "concatString", functionType = "" })) "concatString" "(uncurry (<>))"
    constString s = fromString $ "(const \"" <> s <> "\")"

instance PrimitiveFile HS where
    readFile' = mkCode [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just (Export { module' = "Control.Category.Primitive.File", functionName = "readFile'", functionType = "" })) "readFile'" "(Kleisli $ liftIO . readFile)"
    writeFile' = mkCode [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just (Export { module' = "Control.Category.Primitive.File", functionName = "writeFile'", functionType = "" })) "writeFile'" "(Kleisli $ liftIO . uncurry writeFile)"

instance PrimitiveString HS where
    reverseString = mkCode [("Control.Arrow", [("arr", Nothing)])] (Just (Export { module' = "Control.Category.Primitive.String", functionName = "reverseString", functionType = "" })) "reverseString" "(arr reverse)"

instance Numeric HS where
    num n = fromString $ "(const " <> show n <> ")"
    negate' = "negate"
    add = mkCode [] (Just (Export { module' = "Control.Category.Numeric", functionName = "add", functionType = "" })) "add" "(uncurry (+))"
    mult = mkCode [] (Just (Export { module' = "Control.Category.Numeric", functionName = "mult", functionType = "" })) "mult" "(uncurry (*))"
    div' = mkCode [] (Just (Export { module' = "Control.Category.Numeric", functionName = "div'", functionType = "" })) "div'" "(uncurry div)"
    mod' = mkCode [] (Just (Export { module' = "Control.Category.Numeric", functionName = "mod'", functionType = "" })) "mod'" "(uncurry mod)"

-- I don't quite know how to call ghci or cabal repl to include the correct functions here, so the tests are skipped.

-- @TODO escape shell - Text.ShellEscape?
instance ExecuteHaskellWithDefinitions HS where
    executeViaGHCiWithDefinitions cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementWithDefinitions cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)


instance ExecuteHaskellWithShorthand HS where
    executeViaGHCiWithShorthand cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementWithShorthand cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

instance ExecuteHaskellWithImports HS where
    executeViaGHCiWithImports cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementWithShorthand cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)


-- @TODO this passes too many arguments apparently...
-- This is because of the id and (.) using the (->) instance whereas I am running Kleisli below.
-- This means we need to deal with both within Haskell sessions. Let's try to use Pure/Monadic... or maybe HSPure / HSMonadic accepting only appropriate typeclasses / primitives?
instance ExecuteStdioWithDefinitions HS where
    executeViaStdioWithDefinitions cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XGHC2024"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", BSL.unpack (renderStatementWithDefinitions cat) <> " ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

instance ExecuteStdioWithImports HS where
    executeViaStdioWithImports cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XGHC2024"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toCLIImports cat <>
                [
                "-e", BSL.unpack (renderStatementWithShorthand cat) <> " ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

instance ExecuteStdioWithShorthand HS where
    executeViaStdioWithShorthand cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XGHC2024"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", BSL.unpack (renderStatementWithShorthand cat) <> " ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

-- @ TODO JSON
