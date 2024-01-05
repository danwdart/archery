{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module Data.Code.JS where

import Control.Category
-- import Control.Category.Apply
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Numeric
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Control.Category.Primitive.File
import Control.Category.Primitive.String
import Control.Category.Strong
import Control.Category.Symmetric
import Data.ByteString.Lazy.Char8         qualified as BSL
import Data.Map
import Data.Map                           qualified as M
import Data.Render.File.WithImports
import Data.Render.Statement
import Data.Set
import Data.Set                           qualified as S
import Data.String
import Prelude                            hiding (id, (.))

type Module = BSL.ByteString

type ImportedFunctionName = BSL.ByteString

type RenderedStatement = BSL.ByteString

data JS a b = JS {
    imports :: Map Module (Set ImportedFunctionName),
    code    :: RenderedStatement
} deriving (Eq, Show)

-- toCLIImports ∷ JS a b → [String]
-- toCLIImports (JS imports _) = S.toList imports >>= \importStr -> ["-e", "const {} " <> BSL.unpack importStr]

toFileImports ∷ JS a b → [BSL.ByteString]
toFileImports JS { imports = is } = (\(file, imports) -> "import { " <> BSL.intercalate ", " (S.toList imports) <> " } from \"" <> file <> "\";") <$> M.toList is

fromLib ∷ Module → ImportedFunctionName → RenderedStatement → JS a b
fromLib lib fnName = JS [(lib, [fnName])]

fromLibFn ∷ Module → ImportedFunctionName → JS a b
fromLibFn lib fnName = fromLib lib fnName $ "(" <> fnName <> ")"

addLib ∷ Module → ImportedFunctionName → (RenderedStatement → RenderedStatement) → JS a b → JS c d
addLib lib fnName wrapper p@JS { imports = is } = JS ([(lib, [fnName])] <> is) (wrapper (renderStatement p))

addLibWrapWith ∷ Module → ImportedFunctionName → JS a b → JS c d
addLibWrapWith lib fnName = addLib lib fnName ((("((" <> fnName <> ")") <>) . (<> ")"))

instance IsString (JS a b) where
    fromString = JS [] . BSL.pack

instance RenderStatement (JS a b) where
    renderStatement JS { code = f } = f

instance RenderFileWithImports (JS a b) where
    renderFileWithImports p =
        BSL.unlines (toFileImports p) <>
        "\nexport default " <>
        renderStatement p <>
        ";\n"

-- instance RenderFileWithImports (Map )

instance Bracket JS where
    bracket p@JS { imports = is } = JS is $ "(" <> renderStatement p <> ")"

fromCatFn ∷ BSL.ByteString → JS a b
fromCatFn = fromLibFn "control/category"

instance Category JS where
    id = fromCatFn "id"
    a@JS { imports = isa } . b@JS { imports = isb } = JS (isa <> isb <> [("control/category", ["compose"])]) $ "(compose)(" <> renderStatement a <> ")(" <> renderStatement b <> ")"

fromCartesianFn ∷ ImportedFunctionName → JS a b
fromCartesianFn = fromLibFn "control/category/cartesian"

instance Cartesian JS where
    copy = fromCartesianFn "copy"
    consume = fromCartesianFn "consume"
    fst' = fromCartesianFn "fst"
    snd' = fromCartesianFn "snd"

fromCocartesianFn ∷ ImportedFunctionName → JS a b
fromCocartesianFn = fromLibFn "control/category/cocartesian"

instance Cocartesian JS where
    injectL = fromCocartesianFn "injectL"
    injectR = fromCocartesianFn "injectR"
    unify = fromCocartesianFn "unify"
    tag = fromCocartesianFn "tag"

wrapStrong ∷ ImportedFunctionName → JS a b → JS c d
wrapStrong = addLibWrapWith "control/category/strong"

instance Strong JS where
    first' = wrapStrong "first"
    second' = wrapStrong "second"

wrapChoice ∷ ImportedFunctionName → JS a b → JS c d
wrapChoice = addLibWrapWith "control/category/choice"

instance Choice JS where
    left' = wrapChoice "left"
    right' = wrapChoice "right"

fromSymmetricFn ∷ ImportedFunctionName → JS a b
fromSymmetricFn = fromLibFn "control/category/symmetric"

instance Symmetric JS where
    swap = fromSymmetricFn "swap"
    swapEither = fromSymmetricFn "swapEither"
    reassoc = fromSymmetricFn "reassoc"
    reassocEither = fromSymmetricFn "reassocEither"

-- instance Cochoice JS where

-- instance Costrong JS where

-- instance Apply JS where

instance PrimitiveBool JS where
    eq = fromLibFn "primitive/bool" "eq"

instance PrimitiveConsole JS where
    outputString = fromLibFn "primitive/console" "outputString"
    inputString = fromLibFn "primitive/console" "inputString"

instance PrimitiveExtra JS where
    intToString = fromLibFn "primitive/extra" "intToString"
    concatString = fromLibFn "primitive/extra" "concatString"
    constString = fromLib "primitive/extra" "constString" . ("(\"" <>) . (<> "\")") . BSL.pack

instance PrimitiveFile JS where
    readFile' = fromLibFn "primitive/file" "readFile"
    writeFile' = fromLibFn "primitive/file" "writeFile"

instance PrimitiveString JS where
    reverseString = fromLibFn "primitive/string" "reverseString"

fromNumericFn ∷ ImportedFunctionName → JS a b
fromNumericFn = fromLibFn "control/category/numeric"

instance Numeric JS where
    num = fromLib "control/category/numeric" "num" . ("(" <>) . (<> ")") . BSL.pack . show
    negate' = fromLibFn "control/category/numeric" "negate"
    add = fromNumericFn "add"
    mult = fromNumericFn "mult"
    div' = fromNumericFn "div"
    mod' = fromNumericFn "mod"

-- @TODO escape shell - Text.ShellEscape?
{-}
instance ExecuteJSON JS where
    executeViaJSON cat param = do
        let params ∷ [String]
            params = ["-e", "console.log(JSON.stringify(" <> BSL.unpack (renderStatement cat) <> "(" <> BSL.unpack (encode param) <> ")))"]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case eitherDecode (BSL.pack stdout) of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

instance ExecuteStdio JS where
    executeViaStdio cat stdin = do
        let params ∷ [String]
            params = ["-e", BSL.unpack (renderStatement cat) <> "()"]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params (show stdin))
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case readEither stdout of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret
-}
