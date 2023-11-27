{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe            #-}

module Data.Code.JS.Func where

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
import Data.Render.File
import Data.Render.Statement
import Data.Set qualified as S
import Data.String
import Prelude                            hiding (id, (.))
import Data.Set
import Data.Map

type Module = BSL.ByteString

type ImportedFunctionName = BSL.ByteString

type RenderedStatement = BSL.ByteString

data JSFunc a b = JSFunc {
    imports :: Map Module (Set ImportedFunctionName),
    code    :: RenderedStatement
} deriving (Eq, Show)

-- toCLIImports ∷ JSFunc a b → [String]
-- toCLIImports (JSFunc imports _) = S.toList imports >>= \importStr -> ["-e", "const {} " <> BSL.unpack importStr]

toFileImports :: JSFunc a b -> [BSL.ByteString]
toFileImports JSFunc { imports = is } = fmap (\(file, imports) -> "import { " <> BSL.intercalate ", " (S.toList imports) <> " } from \"" <> file <> "\";") $ M.toList is 

fromLib :: Module -> ImportedFunctionName -> RenderedStatement -> JSFunc a b
fromLib lib fnName = JSFunc [(lib, [fnName])]

fromLibFn :: Module -> ImportedFunctionName -> JSFunc a b
fromLibFn lib fnName = fromLib lib fnName $ "(" <> fnName <> ")"

addLib :: Module -> ImportedFunctionName -> (RenderedStatement -> RenderedStatement) -> JSFunc a b -> JSFunc c d
addLib lib fnName wrapper p@JSFunc { imports = is } = JSFunc ([(lib, [fnName])] <> is) (wrapper (renderStatement p))

addLibWrapWith :: Module -> ImportedFunctionName -> JSFunc a b -> JSFunc c d
addLibWrapWith lib fnName = addLib lib fnName ((("((" <> fnName <> ")") <>) . (<> ")"))

instance IsString (JSFunc a b) where
    fromString = JSFunc [] . BSL.pack

instance RenderStatement (JSFunc a b) where
    renderStatement JSFunc { code = f } = f

instance RenderFile (JSFunc a b) where
    renderFile p =
        BSL.unlines (toFileImports p) <>
        "\nexport default " <>
        renderStatement p <>
        ";\n"
    
-- instance RenderFile (Map )

instance Bracket JSFunc where
    bracket p@JSFunc { imports = is } = JSFunc is $ "(" <> renderStatement p <> ")"

fromCatFn :: BSL.ByteString -> JSFunc a b
fromCatFn = fromLibFn "control/category"

instance Category JSFunc where
    id = fromCatFn "id"
    a@JSFunc { imports = isa } . b@JSFunc { imports = isb } = JSFunc (isa <> isb <> [("control/category", ["compose"])]) $ "(compose)(" <> renderStatement a <> ")(" <> renderStatement b <> ")"

fromCartesianFn :: ImportedFunctionName -> JSFunc a b
fromCartesianFn = fromLibFn "control/category/cartesian"

instance Cartesian JSFunc where
    copy = fromCartesianFn "copy"
    consume = fromCartesianFn "consume"
    fst' = fromCartesianFn "fst"
    snd' = fromCartesianFn "snd"

fromCocartesianFn :: ImportedFunctionName -> JSFunc a b
fromCocartesianFn = fromLibFn "control/category/cocartesian"

instance Cocartesian JSFunc where
    injectL = fromCocartesianFn "injectL"
    injectR = fromCocartesianFn "injectR"
    unify = fromCocartesianFn "unify"
    tag = fromCocartesianFn "tag"

wrapStrong :: ImportedFunctionName -> JSFunc a b -> JSFunc c d
wrapStrong = addLibWrapWith "control/category/strong"

instance Strong JSFunc where
    first' = wrapStrong "first"
    second' = wrapStrong "second"

wrapChoice :: ImportedFunctionName -> JSFunc a b -> JSFunc c d
wrapChoice = addLibWrapWith "control/category/choice"

instance Choice JSFunc where
    left' = wrapChoice "left"
    right' = wrapChoice "right"

fromSymmetricFn :: ImportedFunctionName -> JSFunc a b
fromSymmetricFn = fromLibFn "control/category/symmetric"

instance Symmetric JSFunc where
    swap = fromSymmetricFn "swap"
    swapEither = fromSymmetricFn "swapEither"
    reassoc = fromSymmetricFn "reassoc"
    reassocEither = fromSymmetricFn "reassocEither"

-- instance Cochoice JSFunc where

-- instance Costrong JSFunc where

-- instance Apply JSFunc where

instance PrimitiveBool JSFunc where
    eq = fromLibFn "primitive/bool" "eq"

instance PrimitiveConsole JSFunc where
    outputString = fromLibFn "primitive/console" "outputString"
    inputString = fromLibFn "primitive/console" "inputString"

instance PrimitiveExtra JSFunc where
    intToString = fromLibFn "primitive/extra" "intToString"
    concatString = fromLibFn "primitive/extra" "concatString"
    constString = fromLib "primitive/extra" "constString" . ("(\"" <>) . (<> "\")") . BSL.pack

instance PrimitiveFile JSFunc where
    readFile' = fromLibFn "primitive/file" "readFile"
    writeFile' = fromLibFn "primitive/file" "writeFile"

instance PrimitiveString JSFunc where
    reverseString = fromLibFn "primitive/string" "reverseString"

fromNumericFn :: ImportedFunctionName -> JSFunc a b
fromNumericFn = fromLibFn "control/category/numeric"

instance Numeric JSFunc where
    num = fromLib "control/category/numeric" "num" . ("(" <>) . (<> ")") . BSL.pack . show
    negate' = fromLibFn "control/category/numeric" "negate"
    add = fromNumericFn "add"
    mult = fromNumericFn "mult"
    div' = fromNumericFn "div"
    mod' = fromNumericFn "mod"

-- @TODO escape shell - Text.ShellEscape?
{-}
instance ExecuteJSON JSFunc where
    executeViaJSON cat param = do
        let params ∷ [String]
            params = ["-e", "console.log(JSON.stringify(" <> BSL.unpack (renderStatement cat) <> "(" <> BSL.unpack (encode param) <> ")))"]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case eitherDecode (BSL.pack stdout) of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

instance ExecuteStdio JSFunc where
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