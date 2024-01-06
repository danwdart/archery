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
-- import Control.Category.Execute.Stdio.WithShorthand
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
import Data.Map                                         (Map)
import Data.Map                                         qualified as M
import Data.Maybe
import Data.Render.File.WithDefinitions
import Data.Render.File.WithImports
import Data.Render.File.WithShorthand
import Data.Render.Statement.WithDefinitions
import Data.Render.Statement.WithShorthand
import Data.Set                                         (Set)
import Data.Set                                         qualified as S
import Data.String
import Data.Typeable
import GHC.IO.Exception
import GHC.IsList
import Prelude                                          hiding (id, (.))
import System.Process
import Text.Read

type Module = BSL.ByteString
type FunctionName = BSL.ByteString
type FunctionType = BSL.ByteString
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

data Export = Export {
    module'      :: Module,
    functionName :: FunctionName,
    functionType :: FunctionType,
    definition   :: Definition
} deriving (Eq, Show)

data HS a b = HS {
    imports   :: Imports,
    export    :: Maybe Export,
    shorthand :: Shorthand
} deriving (Eq, Show)

toCLIImports ∷ HS a b → [String]
toCLIImports HS { imports = Imports is } = M.toList is >>= \(moduleName, _ {-imports'-}) -> [{-}"-e", ":l", BSL.unpack moduleName, -} "-e", BSL.unpack $ "import " <> moduleName {-}<> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")"-}]

toFileImports ∷ HS a b → [BSL.ByteString]
toFileImports HS { imports = Imports is } = (\(moduleName, imports') -> "import " <> moduleName <> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")") <$> M.toList is

filterExternal ∷ Imports → Imports
filterExternal = Imports . M.filter (not . null) . fmap (S.filter (isNothing . snd)) . unImports

toExternalFileImports ∷ HS a b → [BSL.ByteString]
toExternalFileImports HS { imports = is } = (\(moduleName, imports') -> "import " <> moduleName <> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")") <$> M.toList (unImports $ filterExternal is)

exportToImports ∷ Maybe Export → Imports
exportToImports = maybe mempty (\Export { module' = module', functionName = functionName', definition = definition'} -> Imports [(module', [(functionName', Just definition')])])

renderDefinitions ∷ HS a b → [BSL.ByteString]
renderDefinitions HS { imports = Imports is } = M.toList is >>= (fmap (\(fnName', mDefinition) -> case mDefinition of
    Nothing          -> ""
    Just definition' -> "\n" <> fnName' <> " = " <> definition' -- TODO types
    ) . S.toList . snd)

instance IsString (HS a b) where
    fromString = HS [] Nothing . BSL.pack

instance RenderStatementWithDefinitions (HS a b) where
    renderStatementWithDefinitions HS { export = export', shorthand = s } = case export' of
        Nothing                                  -> s
        Just Export { definition = definition' } -> definition' -- @TODO is

instance RenderStatementWithShorthand (HS a b) where
    renderStatementWithShorthand HS { shorthand = s } = s -- @TODO is

-- TODO name this
instance (Typeable a, Typeable b) ⇒ RenderFileWithShorthand (HS a b) where
    renderFileWithShorthand cat@HS { export = export' } =
        "{-# LANGUAGE LambdaCase #-}" <>
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
                    functionType = "IO ()",
                    definition = ""
                }
                ) export'

instance (Typeable a, Typeable b) ⇒ RenderFileWithDefinitions (HS a b) where
    renderFileWithDefinitions cat@HS { export = export' } =
        "{-# LANGUAGE LambdaCase #-}" <>
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
                    functionType = "IO ()",
                    definition = ""
                }
                ) export'

instance (Typeable a, Typeable b) ⇒ RenderFileWithImports (HS a b) where
    renderFileWithImports cat@HS { export = export' } =
        "{-# LANGUAGE LambdaCase #-}" <>
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
                    functionType = "IO ()",
                    definition = ""
                }
                ) export'

instance Bracket HS where
    bracket s = s { shorthand = "(" <> renderStatementWithShorthand s <> ")" }

instance Category HS where
    id = "id"
    a . b = HS (imports a <> imports b <> exportToImports (export a) <> exportToImports (export b)) Nothing $ "(" <> renderStatementWithDefinitions a <> " . " <> renderStatementWithDefinitions b <> ")"

instance Cartesian HS where
    copy = HS [] (Just (Export { module' = "Control.Category.Cartesian", functionName = "copy", functionType = "", definition = "\\x -> (x, x)" })) "copy"
    consume = HS [] (Just (Export { module' = "Control.Category.Cartesian", functionName = "consume", functionType = "", definition = "\\x -> ()" })) "consume"
    fst' = "fst"
    snd' = "snd"

instance Cocartesian HS where
    injectL = "Left"
    injectR = "Right"
    unify = HS [] (Just (Export { module' = "Control.Category.Cocartesian", functionName = "unify", functionType = "", definition = "\\case { Left a -> a; Right a -> a; }" })) "unify"
    tag = HS [] (Just (Export { module' = "Control.Category.Cartesian", functionName = "tag", functionType = "", definition = "\\case { (False, a) -> Left a; (True, a) -> Right a; }" })) "tag"

instance Strong HS where
    -- @TODO apply? Bracket?
    first' f = HS ([("Data.Bifunctor", [("first", Nothing)])] <> imports f) Nothing $ "(first " <> renderStatementWithDefinitions f <> ")"
    second' f = HS ([("Data.Bifunctor", [("second", Nothing)])] <> imports f) Nothing $ "(second " <> renderStatementWithDefinitions f <> ")"

instance Choice HS where
    left' f = HS (imports f) Nothing $ "(\\case { Left a -> Left (" <> renderStatementWithDefinitions f <> " a); Right a -> Right a; })"
    right' f = HS (imports f) Nothing $ "(\\case { Left a -> Left a; Right a -> Right (" <> renderStatementWithDefinitions f <> " a); })"

instance Symmetric HS where
    swap = HS [] (Just (Export { module' = "Control.Category.Symmetric", functionName = "swap", functionType = "", definition = "\\(a, b) -> (b, a)" })) "swap"
    swapEither = HS [] (Just (Export { module' = "Control.Category.Symmetric", functionName = "swapEither", functionType = "", definition = "(\\case { Left a -> Right a; Right a -> Left a; })" })) "swapEither"
    reassoc = HS [] (Just (Export { module' = "Control.Category.Symmetric", functionName = "reassoc", functionType = "", definition = "(\\(a, (b, c)) -> ((a, b), c))" })) "reassoc"
    reassocEither = HS [] (Just (Export { module' = "Control.Category.Symmetric", functionName = "reassocEither", functionType = "", definition = "(\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c })" })) "reassocEither"

-- instance Cochoice HS where

-- instance Costrong HS where

-- instance Apply HS where

instance PrimitiveBool HS where
    eq = HS [("Control.Arrow", [("arr", Nothing)])] (Just (Export { module' = "Control.Category.Primitive.Bool", functionName = "eq", functionType = "", definition = "(arr . uncurry $ (==))" })) "eq"

instance PrimitiveConsole HS where
    outputString = HS [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just (Export { module' = "Control.Category.Primitive.Console", functionName = "outputString", functionType = "", definition = "(Kleisli putStr)" })) "outputString"
    inputString = HS [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just (Export { module' = "Control.Category.Primitive.Console", functionName = "inputString", functionType = "", definition = "(Kleisli (const getContents))" })) "inputString"

instance PrimitiveExtra HS where
    intToString = "show"
    concatString = HS [] (Just (Export { module' = "Control.Category.Primitive.Extra", functionName = "concatString", functionType = "", definition = "(uncurry (<>))" })) "concatString"
    constString s = fromString $ "(const \"" <> s <> "\")"

instance PrimitiveFile HS where
    readFile' = HS [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just (Export { module' = "Control.Category.Primitive.File", functionName = "readFile'", functionType = "", definition = "(Kleisli $ liftIO . readFile)" })) "readFile'"
    writeFile' = HS [("Control.Arrow", [("Kleisli(..)", Nothing)])] (Just (Export { module' = "Control.Category.Primitive.File", functionName = "writeFile'", functionType = "", definition = "(Kleisli $ liftIO . uncurry writeFile)" })) "writeFile'"

instance PrimitiveString HS where
    reverseString = HS [("Control.Arrow", [("arr", Nothing)])] (Just (Export { module' = "Control.Category.Primitive.String", functionName = "reverseString", functionType = "", definition = "(arr reverse)" })) "reverseString"

instance Numeric HS where
    num n = fromString $ "(const " <> show n <> ")"
    negate' = "negate"
    add = HS [] (Just (Export { module' = "Control.Category.Numeric", functionName = "add", functionType = "", definition = "(uncurry (+))" })) "add"
    mult = HS [] (Just (Export { module' = "Control.Category.Numeric", functionName = "mult", functionType = "", definition = "(uncurry (*))" })) "mult"
    div' = HS [] (Just (Export { module' = "Control.Category.Numeric", functionName = "div'", functionType = "", definition = "(uncurry div)" })) "div'"
    mod' = HS [] (Just (Export { module' = "Control.Category.Numeric", functionName = "mod'", functionType = "", definition = "(uncurry mod)" })) "mod'"

-- I don't quite know how to call ghci or cabal repl to include the correct functions here, so the tests are skipped.

-- @TODO escape shell - Text.ShellEscape?
instance ExecuteHaskellWithDefinitions HS where
    executeViaGHCiWithDefinitions cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XLambdaCase"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementWithDefinitions cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)


instance ExecuteHaskellWithShorthand HS where
    executeViaGHCiWithShorthand cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XLambdaCase"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementWithShorthand cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

instance ExecuteHaskellWithImports HS where
    executeViaGHCiWithImports cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XLambdaCase"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementWithShorthand cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)


-- @TODO this passes too many arguments apparently...
-- This is because of the id and (.) using the (->) instance whereas I am running Kleisli below.
-- This means we need to deal with both within Haskell sessions. Let's try to use Pure/Monadic... or maybe HSPure / HSMonadic accepting only appropriate typeclasses / primitives?
instance ExecuteStdioWithDefinitions HS where
    executeViaStdioWithDefinitions cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XLambdaCase"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toCLIImports cat <>
                [
                "-e", BSL.unpack (renderStatementWithDefinitions cat) <> " ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

instance ExecuteStdioWithImports HS where
    executeViaStdioWithImports cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XLambdaCase"
                -- "-e", "import Prelude hiding ((.), id)"
                ] <>
                toCLIImports cat <>
                [
                "-e", BSL.unpack (renderStatementWithShorthand cat) <> " ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

-- @ TODO JSON
