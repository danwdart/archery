{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe            #-}

module Data.Code.PHP.Func where

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
import Data.Map qualified as M
import Data.Render.File.WithImports
import Data.Render.Statement
import Data.Set qualified as S
import Data.String
import Prelude                            hiding (id, (.))
import Data.Set
import Data.Map

type Module = BSL.ByteString

type ImportedFunctionName = BSL.ByteString

type RenderedStatement = BSL.ByteString

data PHPFunc a b = PHPFunc {
    imports :: Map Filename (Set ImportedFunctionName),
    code    :: RenderedStatement
} deriving (Eq, Show)

-- toCLIImports ∷ PHPFunc a b → [String]
-- toCLIImports (PHPFunc imports _) = S.toList imports >>= \importStr -> ["-e", "const {} " <> BSL.unpack importStr]

toFileImports :: PHPFunc a b -> [BSL.ByteString]
toFileImports PHPFunc { imports = is } = fmap (\(file, imports) -> BSL.intercalate "\n" (fmap (\imprt -> "use function " <> file <> "\\" <> imprt) $ S.toList imports) <> ";") $ M.toList is 

fromLib :: Module -> ImportedFunctionName -> RenderedStatement -> PHPFunc a b
fromLib lib fnName = PHPFunc [(lib, [fnName])]

fromLibFn :: Module -> ImportedFunctionName -> PHPFunc a b
fromLibFn lib fnName = fromLib lib fnName $ "(" <> fnName <> ")"

addLib :: Module -> ImportedFunctionName -> (RenderedStatement -> RenderedStatement) -> PHPFunc a b -> PHPFunc c d
addLib lib fnName wrapper p@PHPFunc { imports = is } = PHPFunc ([(lib, [fnName])] <> is) (wrapper (renderStatement p))

addLibWrapWith :: Module -> ImportedFunctionName -> PHPFunc a b -> PHPFunc c d
addLibWrapWith lib fnName = addLib lib fnName ((("((" <> fnName <> ")") <>) . (<> ")"))

instance IsString (PHPFunc a b) where
    fromString = PHPFunc [] . BSL.pack

instance RenderStatement (PHPFunc a b) where
    renderStatement PHPFunc { code = f } = f

instance RenderFileWithImports (PHPFunc a b) where
    renderFileWithImports p =
        "<?php\n" <>
        "require(\"vendor/autoload.php\");\n" <>
        BSL.unlines (toFileImports p) <>
        "\nreturn " <>
        renderStatement p <>
        ";\n"

instance Bracket PHPFunc where
    bracket p@PHPFunc { imports = is } = PHPFunc is $ "(" <> renderStatement p <> ")"

fromCatFn :: BSL.ByteString -> PHPFunc a b
fromCatFn = fromLibFn "Control\\Category"

instance Category PHPFunc where
    id = fromCatFn "id"
    a@PHPFunc { imports = isa } . b@PHPFunc { imports = isb } = PHPFunc (isa <> isb <> [("Control\\Category", ["compose"])]) $ "(compose)(" <> renderStatement a <> ")(" <> renderStatement b <> ")"

fromCartesianFn :: Import -> PHPFunc a b
fromCartesianFn = fromLibFn "Control\\Category\\Cartesian"

instance Cartesian PHPFunc where
    copy = fromCartesianFn "copy"
    consume = fromCartesianFn "consume"
    fst' = fromCartesianFn "fst"
    snd' = fromCartesianFn "snd"

fromCocartesianFn :: Import -> PHPFunc a b
fromCocartesianFn = fromLibFn "Control\\Category\\Cocartesian"

instance Cocartesian PHPFunc where
    injectL = fromCocartesianFn "injectL"
    injectR = fromCocartesianFn "injectR"
    unify = fromCocartesianFn "unify"
    tag = fromCocartesianFn "tag"

wrapStrong :: Import -> PHPFunc a b -> PHPFunc c d
wrapStrong = addLibWrapWith "Control\\Category\\Strong"

instance Strong PHPFunc where
    first' = wrapStrong "first"
    second' = wrapStrong "second"

wrapChoice :: Import -> PHPFunc a b -> PHPFunc c d
wrapChoice = addLibWrapWith "Control\\Category\\Choice"

instance Choice PHPFunc where
    left' = wrapChoice "left"
    right' = wrapChoice "right"

fromSymmetricFn :: Import -> PHPFunc a b
fromSymmetricFn = fromLibFn "Control\\Category\\Symmetric"

instance Symmetric PHPFunc where
    swap = fromSymmetricFn "swap"
    swapEither = fromSymmetricFn "swapEither"
    reassoc = fromSymmetricFn "reassoc"
    reassocEither = fromSymmetricFn "reassocEither"

-- instance Cochoice PHPFunc where

-- instance Costrong PHPFunc where

-- instance Apply PHPFunc where

instance PrimitiveBool PHPFunc where
    eq = fromLibFn "Primitive\\Bool" "eq"

instance PrimitiveConsole PHPFunc where
    outputString = fromLibFn "Primitive\\Console" "outputString"
    inputString = fromLibFn "Primitive\\Console" "inputString"

instance PrimitiveExtra PHPFunc where
    intToString = fromLibFn "Primitive\\Extra" "intToString"
    concatString = fromLibFn "Primitive\\Extra" "concatString"
    constString = fromLib "Primitive\\Extra" "constString" . ("(\"" <>) . (<> "\")") . BSL.pack

instance PrimitiveFile PHPFunc where
    readFile' = fromLibFn "Primitive\\File" "readFile"
    writeFile' = fromLibFn "Primitive\\File" "writeFile"

instance PrimitiveString PHPFunc where
    reverseString = fromLibFn "Primitive\\String" "reverseString"

fromNumericFn :: Import -> PHPFunc a b
fromNumericFn = fromLibFn "Control\\Category\\Numeric"

instance Numeric PHPFunc where
    num = fromLib "Control\\Category\\Numeric" "num" . ("(" <>) . (<> ")") . BSL.pack . show
    negate' = fromLibFn "Control\\Category\\Numeric" "negate"
    add = fromNumericFn "add"
    mult = fromNumericFn "mult"
    div' = fromNumericFn "div"
    mod' = fromNumericFn "mod"

-- @TODO escape shell - Text.ShellEscape?
{-}
instance ExecuteJSON PHPFunc where
    executeViaJSON cat param = do
        let params ∷ [String]
            params = ["-e", "console.log(JSON.stringify(" <> BSL.unpack (renderStatement cat) <> "(" <> BSL.unpack (encode param) <> ")))"]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case eitherDecode (BSL.pack stdout) of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

instance ExecuteStdio PHPFunc where
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