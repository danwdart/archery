{- GeneralisedNewtypeDeriving -}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE Unsafe                     #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

-- | Program module. Like Func, but dynamically imports modules as required.
module Data.Code.Haskell where

import Control.Category
-- import Control.Category.Apply
-- import Control.Category.Bracket
import Control.Category.Cartesian
-- import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.Haskell.Longhand
import Control.Category.Execute.Haskell.Imports
import Control.Category.Execute.Haskell.Shorthand
-- import Control.Category.Execute.Stdio.Longhand
-- import Control.Category.Execute.Stdio.Imports
-- import Control.Category.Execute.Stdio.Shorthand
-- import Control.Category.Numeric
-- import Control.Category.Primitive.Bool
-- import Control.Category.Primitive.Console
-- import Control.Category.Primitive.Extra
-- import Control.Category.Primitive.File
-- import Control.Category.Primitive.String
-- import Control.Category.Strong
-- import Control.Category.Symmetric
import Control.Exception                                hiding (bracket)
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8                       qualified as BSL
import Data.Code.Generic
-- import Data.Map                                         (Map)
-- import Data.Map                                         qualified as M
-- import Data.Maybe
import Data.Render.File.Longhand
import Data.Render.File.Imports
import Data.Render.File.Shorthand
import Data.Render.Statement.Longhand
import Data.Render.Statement.Shorthand
-- import Data.Set                                         (Set)
-- import Data.Set                                         qualified as S
-- import Data.String
-- import Data.Typeable
import GHC.IO.Exception
import GHC.IsList
import Prelude                                          hiding (id, (.))
import System.Process
import Text.Read

newtype HS a b = HS {
    _code :: Code a b
} deriving stock (Eq, Show)

instance HasCode HS a b where
    code = _code

instance MkCode HS a b where
    mkCode ei ii m f = HS $ mkCode ei ii m f

{-}
toCLIImports ∷ HS a b → [String]
toCLIImports hs = M.toList (unImports . imports $ hs) >>= \(moduleName, _imports') -> [{-}"-e", ":l", BSL.unpack moduleName,-}"-e", BSL.unpack $ "import " <> moduleName {-<> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")\""-}]
-}
toExternalCLIImports ∷ HS a b → [String]
toExternalCLIImports hs = GHC.IsList.toList (externalImports hs) >>= \(moduleName, _imports' ) -> [{-}"-e", ":l", BSL.unpack moduleName,-} "-e", BSL.unpack $ "import " <> moduleName {-<> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")\""-}]
{-}
toFileImports ∷ HS a b → [BSL.ByteString]
toFileImports hs = (\(moduleName, imports') -> "import " <> moduleName <> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")") <$> M.toList (unImports $ imports hs)

toExternalFileImports ∷ HS a b → [BSL.ByteString]
toExternalFileImports hs = (\(moduleName, imports') -> "import " <> moduleName <> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")") <$> M.toList (unImports $ filterExternal (imports hs))

renderLonghand ∷ HS a b → [BSL.ByteString]
renderLonghand hs = M.toList (unImports $ imports hs) >>= (fmap (\(fnName', mDefinition) -> case mDefinition of
    Nothing          -> ""
    Just longhand' -> "\n" <> fnName' <> " = " <> longhand' -- TODO types
    ) . S.toList . snd)

-}

instance RenderStatementLonghand (HS a b) where
    renderStatementLonghand = longhand

instance RenderStatementShorthand (HS a b) where
    renderStatementShorthand = shorthand

{-}
-- TODO name this
instance (Typeable a, Typeable b) ⇒ RenderFileShorthand (HS a b) where
    renderFileShorthand cat =
        "\nmodule " <> module' <> " where\n\n" <>
        BSL.unlines (renderLonghand cat) <>
        "\n" <> functionName' <> " :: " <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> functionName' <> " = " <> renderStatementShorthand cat where
            Export {
                _module = module',
                _functionName = functionName'
            } = fromMaybe (
                Export {
                    _module = "Main",
                    _functionName = "main",
                    _functionType = "IO ()"
                }
                ) (export cat)

instance (Typeable a, Typeable b) ⇒ RenderFileLonghand (HS a b) where
    renderFileLonghand cat =
        "\nmodule " <> module' <> " where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        "\n" <> functionName' <> " :: " <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> functionName' <> " = " <> renderStatementLonghand cat where
            Export {
                _module = module',
                _functionName = functionName'
            } = fromMaybe (
                Export {
                    _module = "Main",
                    _functionName = "main",
                    _functionType = "IO ()"
                }
                ) (export cat)

instance (Typeable a, Typeable b) ⇒ RenderFileImports (HS a b) where
    renderFileImports cat =
        "\nmodule " <> module' <> " where\n\n" <>
        BSL.unlines (toFileImports cat) <>
        "\n" <> functionName' <> " :: " <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> functionName' <> " = " <> renderStatementShorthand cat where
            Export {
                _module = module',
                _functionName = functionName'
            } = fromMaybe (
                Export {
                    _module = "Main",
                    _functionName = "main",
                    _functionType = "IO ()"
                }
                ) (export cat)

instance Bracket HS where
    bracket hs@(HS s) = HS $ s {
        _shorthand = "(" <> renderStatementShorthand hs <> ")",
        _longhand = "(" <> renderStatementLonghand hs <> ")"
    }
-}

instance Category HS where
    id = HS $ Code {
        _externalImports = [],
        _internalImports = [],
        _module = "Control.Category.Function",
        _function = Function {
            _functionName = "id",
            _functionTypeFrom = "a",
            _functionTypeTo = "a",
            _shorthand = "id",
            _longhand = "\\x -> x"
        }
    }
    a . b = HS $ Code {
        _externalImports = externalImports a <> externalImports b,
        _internalImports = internalImports a <> internalImports b <> [
            (module' a, [ function a ])
        ] <> [
            (module' b, [ function b ])
        ],
        _module = "Control.Category.Function",
        _function = Function {
            _functionName = "(.)",
            -- TODO Typeable
            _functionTypeFrom = "(" <> functionTypeFrom b <> " -> " <> functionTypeTo b <> ") -> (" <> functionTypeFrom a <> " -> " <> functionTypeTo a <> ")",
            _functionTypeTo = functionTypeFrom b <> " -> " <> functionTypeTo a,
            _shorthand = "(" <> shorthand a <> " . " <> shorthand b <> ")",
            _longhand = "(" <> longhand a <> " . " <> longhand b <> ")"
        }
    }

instance Cartesian HS where
    copy = HS $ Code {
        _externalImports = [],
        _internalImports = [],
        _module = "Control.Category.Cartesian",
        _function = Function {
            _functionName = "copy",
            _functionTypeFrom = "a",
            _functionTypeTo = "(a, a)",
            _shorthand = "\\x -> (x, x)",
            _longhand = "\\x -> (x, x)"
        }
    }
    consume = HS $ Code {
        _externalImports = [],
        _internalImports = [],
        _module = "Control.Category.Cartesian",
        _function = Function {
            _functionName = "consume",
            _functionTypeFrom = "a",
            _functionTypeTo = "()",
            _shorthand = "consume",
            _longhand = "\\x -> ()"
        }
    }
    fst' = HS $ Code {
        _externalImports = [],
        _internalImports = [],
        _module = "Control.Category.Cartesian",
        _function = Function {
            _functionName = "fst",
            _functionTypeFrom = "(a, b)",
            _functionTypeTo = "a",
            _shorthand = "fst",
            _longhand = "\\(a, b) -> a"
        }
    }
    snd' = HS $ Code {
        _externalImports = [],
        _internalImports = [],
        _module = "Control.Category.Cartesian",
        _function = Function {
            _functionName = "snd",
            _functionTypeFrom = "(a, b)",
            _functionTypeTo = "a",
            _shorthand = "snd",
            _longhand = "\\(a, b) -> b"
        }
    }

instance Cocartesian HS where
    injectL = HS $ Code {
        _externalImports = [],
        _internalImports = [],
        _module = "Control.Category.Cocartesian",
        _function = Function {
            _functionName = "injectL",
            _functionTypeFrom = "a",
            _functionTypeTo = "Either a b",
            _shorthand = "Left",
            _longhand = "\\a -> Left a"
        }
    }
    injectR = HS $ Code {
        _externalImports = [],
        _internalImports = [],
        _module = "Control.Category.Cocartesian",
        _function = Function {
            _functionName = "injectR",
            _functionTypeFrom = "b",
            _functionTypeTo = "Either a b",
            _shorthand = "Right",
            _longhand = "\\b -> Right b"
        }
    }
    unify = HS $ Code {
        _externalImports = [],
        _internalImports = [],
        _module = "Control.Category.Cocartesian",
        _function = Function {
            _functionName = "unify",
            _functionTypeFrom = "Either a a",
            _functionTypeTo = "a",
            _shorthand = "unify",
            _longhand = "\\case { Left a -> a; Right a -> a; }"
        }
    }
    tag = HS $ Code {
        _externalImports = [],
        _internalImports = [],
        _module = "Control.Category.Cocartesian",
        _function = Function {
            _functionName = "unify",
            _functionTypeFrom = "(Bool, a)",
            _functionTypeTo = "Either a a",
            _shorthand = "tag",
            _longhand = "\\case { (False, a) -> Left a; (True, a) -> Right a; }"
        }
    }

-- >>> import Control.Category
-- >>> ((Control.Category..) fst' copy) :: HS String String
-- HS {_code = Code {_externalImports = MapSet {getMapSet = fromList []}, _internalImports = MapSet {getMapSet = fromList [("Control.Category.Cartesian",fromList [Function {_functionName = "copy", _functionTypeFrom = "a", _functionTypeTo = "(a, a)", _shorthand = "\\x -> (x, x)", _longhand = "\\x -> (x, x)"},Function {_functionName = "fst", _functionTypeFrom = "(a, b)", _functionTypeTo = "a", _shorthand = "fst", _longhand = "\\(a, b) -> a"}])]}, _module = "Control.Category.Function", _function = Function {_functionName = "(.)", _functionTypeFrom = "(a -> (a, a)) -> ((a, b) -> a)", _functionTypeTo = "a -> a", _shorthand = "(fst . \\x -> (x, x))", _longhand = "(\\(a, b) -> a . \\x -> (x, x))"}}}

-- >>> renderStatementLonghand (((Control.Category..) fst' copy) :: HS String String)


{-}
instance Strong HS where
    -- @TODO apply? Bracket?
    first' f = HS $ Code {
        _imports = [("Data.Bifunctor", [("first", Nothing)])] <> imports f,
        _export = Nothing,
        _shorthand = "(Data.Bifunctor.first (" <> renderStatementShorthand f <> "))",
        _longhand = "(Data.Bifunctor.first (" <> renderStatementLonghand f <> "))"
    }
    second' f = HS $ Code {
        _imports = [("Data.Bifunctor", [("second", Nothing)])] <> imports f,
        _export = Nothing,
        _shorthand = "(Data.Bifunctor.second (" <> renderStatementShorthand f <> "))",
        _longhand = "(Data.Bifunctor.second (" <> renderStatementLonghand f <> "))"
    }

instance Choice HS where
    left' f = HS $ Code {
        _imports = imports f,
        _export = Nothing,
        _shorthand = "(\\case { Left a -> Left ((" <> renderStatementShorthand f <> ") a); Right a -> Right a; })",
        _longhand = "(\\case { Left a -> Left ((" <> renderStatementLonghand f <> ") a); Right a -> Right a; })"
    }
    right' f = HS $ Code {
        _imports = imports f,
        _export = Nothing,
        _shorthand = "(\\case { Left a -> Left a; Right a -> Right ((" <> renderStatementShorthand f <> ") a); })",
        _longhand = "(\\case { Left a -> Left a; Right a -> Right ((" <> renderStatementLonghand f <> ") a); })"
    }

instance Symmetric HS where
    swap = mkCode [] (Just (Export { _module = "Control.Category.Symmetric", _functionName = "swap", _functionType = "" })) "swap" "\\(a, b) -> (b, a)"
    swapEither = mkCode [] (Just (Export { _module = "Control.Category.Symmetric", _functionName = "swapEither", _functionType = "" })) "swapEither" "(\\case { Left a -> Right a; Right a -> Left a; })"
    reassoc = mkCode [] (Just (Export { _module = "Control.Category.Symmetric", _functionName = "reassoc", _functionType = "" })) "reassoc" "(\\(a, (b, c)) -> ((a, b), c))"
    reassocEither = mkCode [] (Just (Export { _module = "Control.Category.Symmetric", _functionName = "reassocEither", _functionType = "" })) "reassocEither" "(\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c })"

-- instance Cochoice HS where

-- instance Costrong HS where

-- instance Apply HS where

instance PrimitiveBool HS where
    eq = mkCode [("Control.Arrow", [("arr", Nothing)])] (Just (Export { _module = "Control.Category.Primitive.Bool", _functionName = "eq", _functionType = "" })) "eq" "(arr . uncurry $ (==))"

instance PrimitiveConsole HS where
    outputString = mkCode [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just (Export { _module = "Control.Category.Primitive.Console", _functionName = "outputString", _functionType = "" })) "outputString" "(Kleisli putStr)"
    inputString = mkCode [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just (Export { _module = "Control.Category.Primitive.Console", _functionName = "inputString", _functionType = "" })) "inputString" "(Kleisli (const getContents))"

instance PrimitiveExtra HS where
    intToString :: HS Int String
    intToString = "show"
    concatString = mkCode [] (Just (Export { _module = "Control.Category.Primitive.Extra", _functionName = "concatString", _functionType = "" })) "concatString" "(uncurry (<>))"
    constString s = fromString $ "(const \"" <> s <> "\")"

instance PrimitiveFile HS where
    readFile' = mkCode [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just (Export { _module = "Control.Category.Primitive.File", _functionName = "readFile'", _functionType = "" })) "readFile'" "(Kleisli $ liftIO . readFile)"
    writeFile' = mkCode [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just (Export { _module = "Control.Category.Primitive.File", _functionName = "writeFile'", _functionType = "" })) "writeFile'" "(Kleisli $ liftIO . uncurry writeFile)"

instance PrimitiveString HS where
    reverseString = mkCode [("Control.Arrow", [("arr", Nothing)])] (Just (Export { _module = "Control.Category.Primitive.String", _functionName = "reverseString", _functionType = "" })) "reverseString" "(arr reverse)"

instance Numeric HS where
    num n = fromString $ "(const " <> show n <> ")"
    negate' = "negate"
    add = mkCode [] (Just (Export { _module = "Control.Category.Numeric", _functionName = "add", _functionType = "" })) "add" "(uncurry (+))"
    mult = mkCode [] (Just (Export { _module = "Control.Category.Numeric", _functionName = "mult", _functionType = "" })) "mult" "(uncurry (*))"
    div' = mkCode [] (Just (Export { _module = "Control.Category.Numeric", _functionName = "div'", _functionType = "" })) "div'" "(uncurry div)"
    mod' = mkCode [] (Just (Export { _module = "Control.Category.Numeric", _functionName = "mod'", _functionType = "" })) "mod'" "(uncurry mod)"

-- I don't quite know how to call ghci or cabal repl to include the correct functions here, so the tests are skipped.

-- @TODO escape shell - Text.ShellEscape?
-}

instance ExecuteHaskellLonghand HS where
    executeGHCiLonghand cat param = do
        let params ∷ [String]
            params = [
                -- "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementLonghand cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

instance ExecuteHaskellShorthand HS where
    executeGHCiShorthand cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementShorthand cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

{-
instance ExecuteHaskellImports HS where
    executeGHCiImports cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementShorthand cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)


-- @TODO this passes too many arguments apparently...
-- This is because of the id and (.) using the (->) instance whereas I am running Kleisli below.
-- This means we need to deal with both within Haskell sessions. Let's try to use Pure/Monadic... or maybe HSPure / HSMonadic accepting only appropriate typeclasses / primitives?
instance ExecuteStdioLonghand HS where
    executeStdioLonghand cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XGHC2024"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", BSL.unpack (renderStatementLonghand cat) <> " ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

instance ExecuteStdioImports HS where
    executeStdioImports cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XGHC2024"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toCLIImports cat <>
                [
                "-e", BSL.unpack (renderStatementShorthand cat) <> " ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

instance ExecuteStdioShorthand HS where
    executeStdioShorthand cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XGHC2024"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", BSL.unpack (renderStatementShorthand cat) <> " ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

-- @ TODO JSON
-}
