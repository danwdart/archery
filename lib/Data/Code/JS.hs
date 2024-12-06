{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe              #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-safe -Wno-unused-imports #-}

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
import Data.Code.Generic
import Data.Map
import Data.Map                           qualified as M
import Data.Render.File.WithDefinitions
import Data.Render.File.WithImports
import Data.Render.File.WithShorthand
import Data.Render.Statement.WithDefinitions
import Data.Render.Statement.WithShorthand
import Data.Set
import Data.Set                           qualified as S
import Data.String
import Prelude                            hiding (id, (.))

newtype JS a b = JS {
    _code :: Code a b
} deriving stock (Eq, Show)

instance HasCode JS a b where
    code = _code

{-}
instance MkCode JS a b where
    mkCode i e s d = JS $ mkCode i e s d
-}
-- toCLIImports ∷ JS a b → [String]
-- toCLIImports (JS imports _) = S.toList imports >>= \importStr -> ["-e", "const {} " <> BSL.unpack importStr]

-- toFileImports ∷ JS a b → [BSL.ByteString]
-- toFileImports js = (\(file, imports') -> "import { " <> BSL.intercalate ", " (S.toList imports') <> " } from \"" <> file <> "\";") <$> M.toList (_ $ imports js)

-- fromLib ∷ Module → FunctionName → RenderedStatement → JS a b
-- fromLib lib fnName = JS [(lib, [fnName])]
-- 
-- fromLibFn ∷ Module → FunctionName → JS a b
-- fromLibFn lib fnName = fromLib lib fnName $ "(" <> fnName <> ")"
-- 
-- addLib ∷ Module → FunctionName → (RenderedStatement → RenderedStatement) → JS a b → JS c d
-- addLib lib fnName wrapper p@JS { imports = is } = JS ([(lib, [fnName])] <> is) (wrapper (renderStatementWithDefinitions p))
-- 
-- addLibWrapWith ∷ Module → FunctionName → JS a b → JS c d
-- addLibWrapWith lib fnName = addLib lib fnName ((("((" <> fnName <> ")") <>) . (<> ")"))
-- 
-- instance IsString (JS a b) where
--     fromString = JS [] . BSL.pack

-- instance RenderStatementWithShorthand (JS a b) where
--     renderStatement JS { code = f } = f

{-}
instance RenderStatementWithDefinitions (JS a b) where
    renderStatementWithDefinitions = definition
-}
-- instance RenderFileWithImports (JS a b) where
--     renderFileWithImports p =
--         BSL.unlines (toFileImports p) <>
--         "\nexport default " <>
--         renderStatement p <>
--         ";\n"

-- instance RenderFileWithImports (Map )

-- instance Bracket JS where
    -- bracket p@JS { imports = is } = JS is $ "(" <> renderStatement p <> ")"

{-}
instance Category JS where
    id = mkCode (Imports []) (Just (Export "control/category" "id" "a => a")) "id" "x => x"
    -- TODO check that this definition can't just be (a => b => c => a (b (c)))
    -- e.g. 
    a . b = mkCode (imports a <> imports b) (Just (Export "control/category" "compose" "any")) ("(compose)(" <> shorthand a <> ")(" <> shorthand b <> ")") ("()") 
--     id = fromCatFn "id"
--     a@JS { imports = isa } . b@JS { imports = isb } = JS (isa <> isb <> [("control/category", ["compose"])]) $ "(compose)(" <> renderStatement a <> ")(" <> renderStatement b <> ")"-- 
-}
-- fromCartesianFn ∷ ImportedFunctionName → JS a b
-- fromCartesianFn = fromLibFn "control/category/cartesian"-- 

-- instance Cartesian JS where
--     copy = fromCartesianFn "copy"
--     consume = fromCartesianFn "consume"
--     fst' = fromCartesianFn "fst"
--     snd' = fromCartesianFn "snd"-- 

-- fromCocartesianFn ∷ ImportedFunctionName → JS a b
-- fromCocartesianFn = fromLibFn "control/category/cocartesian"-- 

-- instance Cocartesian JS where
--     injectL = fromCocartesianFn "injectL"
--     injectR = fromCocartesianFn "injectR"
--     unify = fromCocartesianFn "unify"
--     tag = fromCocartesianFn "tag"-- 

-- wrapStrong ∷ ImportedFunctionName → JS a b → JS c d
-- wrapStrong = addLibWrapWith "control/category/strong"-- 

-- instance Strong JS where
--     first' = wrapStrong "first"
--     second' = wrapStrong "second"-- 

-- wrapChoice ∷ ImportedFunctionName → JS a b → JS c d
-- wrapChoice = addLibWrapWith "control/category/choice"-- 

-- instance Choice JS where
--     left' = wrapChoice "left"
--     right' = wrapChoice "right"-- 

-- fromSymmetricFn ∷ ImportedFunctionName → JS a b
-- fromSymmetricFn = fromLibFn "control/category/symmetric"-- 

-- instance Symmetric JS where
--     swap = fromSymmetricFn "swap"
--     swapEither = fromSymmetricFn "swapEither"
--     reassoc = fromSymmetricFn "reassoc"
--     reassocEither = fromSymmetricFn "reassocEither"-- 

-- -- instance Cochoice JS where-- 

-- -- instance Costrong JS where-- 

-- -- instance Apply JS where-- 

-- instance PrimitiveBool JS where
--     eq = fromLibFn "primitive/bool" "eq"-- 

-- instance PrimitiveConsole JS where
--     outputString = fromLibFn "primitive/console" "outputString"
--     inputString = fromLibFn "primitive/console" "inputString"-- 

-- instance PrimitiveExtra JS where
--     intToString = fromLibFn "primitive/extra" "intToString"
--     concatString = fromLibFn "primitive/extra" "concatString"
--     constString = fromLib "primitive/extra" "constString" . ("(\"" <>) . (<> "\")") . BSL.pack-- 

-- instance PrimitiveFile JS where
--     readFile' = fromLibFn "primitive/file" "readFile"
--     writeFile' = fromLibFn "primitive/file" "writeFile"-- 

-- instance PrimitiveString JS where
--     reverseString = fromLibFn "primitive/string" "reverseString"-- 

-- fromNumericFn ∷ ImportedFunctionName → JS a b
-- fromNumericFn = fromLibFn "control/category/numeric"-- 

-- instance Numeric JS where
--     num = fromLib "control/category/numeric" "num" . ("(" <>) . (<> ")") . BSL.pack . show
--     negate' = fromLibFn "control/category/numeric" "negate"
--     add = fromNumericFn "add"
--     mult = fromNumericFn "mult"
--     div' = fromNumericFn "div"
--     mod' = fromNumericFn "mod"

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
