{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

module Data.Code.PHP where

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

data PHP a b = PHP {
    imports :: Map Filename (Set ImportedFunctionName),
    code    :: RenderedStatement
} deriving (Eq, Show)

-- toCLIImports ∷ PHP a b → [String]
-- toCLIImports (PHP imports _) = S.toList imports >>= \importStr -> ["-e", "const {} " <> BSL.unpack importStr]

toFileImports ∷ PHP a b → [BSL.ByteString]
toFileImports PHP { imports = is } = (\(file, imports) -> BSL.intercalate "\n" ((\imprt -> "use function " <> file <> "\\" <> imprt) <$> S.toList imports) <> ";") <$> M.toList is

fromLib ∷ Module → ImportedFunctionName → RenderedStatement → PHP a b
fromLib lib fnName = PHP [(lib, [fnName])]

fromLibFn ∷ Module → ImportedFunctionName → PHP a b
fromLibFn lib fnName = fromLib lib fnName $ "(" <> fnName <> ")"

addLib ∷ Module → ImportedFunctionName → (RenderedStatement → RenderedStatement) → PHP a b → PHP c d
addLib lib fnName wrapper p@PHP { imports = is } = PHP ([(lib, [fnName])] <> is) (wrapper (renderStatement p))

addLibWrapWith ∷ Module → ImportedFunctionName → PHP a b → PHP c d
addLibWrapWith lib fnName = addLib lib fnName ((("((" <> fnName <> ")") <>) . (<> ")"))

instance IsString (PHP a b) where
    fromString = PHP [] . BSL.pack

instance RenderStatement (PHP a b) where
    renderStatement PHP { code = f } = f

instance RenderFileWithImports (PHP a b) where
    renderFileWithImports p =
        "<?php\n" <>
        "require(\"vendor/autoload.php\");\n" <>
        BSL.unlines (toFileImports p) <>
        "\nreturn " <>
        renderStatement p <>
        ";\n"

instance Bracket PHP where
    bracket p@PHP { imports = is } = PHP is $ "(" <> renderStatement p <> ")"

fromCatFn ∷ BSL.ByteString → PHP a b
fromCatFn = fromLibFn "Control\\Category"

instance Category PHP where
    id = fromCatFn "id"
    a@PHP { imports = isa } . b@PHP { imports = isb } = PHP (isa <> isb <> [("Control\\Category", ["compose"])]) $ "(compose)(" <> renderStatement a <> ")(" <> renderStatement b <> ")"

fromCartesianFn ∷ Import → PHP a b
fromCartesianFn = fromLibFn "Control\\Category\\Cartesian"

instance Cartesian PHP where
    copy = fromCartesianFn "copy"
    consume = fromCartesianFn "consume"
    fst' = fromCartesianFn "fst"
    snd' = fromCartesianFn "snd"

fromCocartesianFn ∷ Import → PHP a b
fromCocartesianFn = fromLibFn "Control\\Category\\Cocartesian"

instance Cocartesian PHP where
    injectL = fromCocartesianFn "injectL"
    injectR = fromCocartesianFn "injectR"
    unify = fromCocartesianFn "unify"
    tag = fromCocartesianFn "tag"

wrapStrong ∷ Import → PHP a b → PHP c d
wrapStrong = addLibWrapWith "Control\\Category\\Strong"

instance Strong PHP where
    first' = wrapStrong "first"
    second' = wrapStrong "second"

instance Choice PHP where
    left' = wrapChoice "left"
    right' = wrapChoice "right"

instance Symmetric PHP where
    swap = fromSymmetricFn "swap"
    swapEither = fromSymmetricFn "swapEither"
    reassoc = fromSymmetricFn "reassoc"
    reassocEither = fromSymmetricFn "reassocEither"

-- instance Cochoice PHP where

-- instance Costrong PHP where

-- instance Apply PHP where

instance PrimitiveBool PHP where
    eq = fromLibFn "Primitive\\Bool" "eq"

instance PrimitiveConsole PHP where
    outputString = fromLibFn "Primitive\\Console" "outputString"
    inputString = fromLibFn "Primitive\\Console" "inputString"

instance PrimitiveExtra PHP where
    intToString = fromLibFn "Primitive\\Extra" "intToString"
    concatString = fromLibFn "Primitive\\Extra" "concatString"
    constString = fromLib "Primitive\\Extra" "constString" . ("(\"" <>) . (<> "\")") . BSL.pack

instance PrimitiveFile PHP where
    readFile' = fromLibFn "Primitive\\File" "readFile"
    writeFile' = fromLibFn "Primitive\\File" "writeFile"

instance PrimitiveString PHP where
    reverseString = fromLibFn "Primitive\\String" "reverseString"

fromNumericFn ∷ Import → PHP a b
fromNumericFn = fromLibFn "Control\\Category\\Numeric"

instance Numeric PHP where
    num = fromLib "Control\\Category\\Numeric" "num" . ("(" <>) . (<> ")") . BSL.pack . show
    negate' = fromLibFn "Control\\Category\\Numeric" "negate"
    add = fromNumericFn "add"
    mult = fromNumericFn "mult"
    div' = fromNumericFn "div"
    mod' = fromNumericFn "mod"

-- @TODO escape shell - Text.ShellEscape?
{-}
instance ExecuteJSON PHP where
    executeViaJSON cat param = do
        let params ∷ [String]
            params = ["-e", "console.log(JSON.stringify(" <> BSL.unpack (renderStatement cat) <> "(" <> BSL.unpack (encode param) <> ")))"]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case eitherDecode (BSL.pack stdout) of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

instance ExecuteStdio PHP where
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
