{- GeneralisedNewtypeDeriving -}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe               #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

-- | Program module. Like Func, but dynamically imports modules as required.
module Data.Code.JS where

import Control.Category
-- import Control.Category.Apply
-- import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
-- import Control.Category.Execute.JS.Imports
-- import Control.Category.Execute.JS.Longhand
-- import Control.Category.Execute.JS.Shorthand
import Control.Category.Execute.JSON.Imports
import Control.Category.Execute.JSON.Longhand
import Control.Category.Execute.JSON.Shorthand
import Control.Category.Execute.Stdio.Imports
import Control.Category.Execute.Stdio.Longhand
import Control.Category.Execute.Stdio.Shorthand
import Control.Category.Numeric
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
-- import Control.Category.Primitive.File
import Control.Category.Primitive.String
import Control.Category.Strong
import Control.Category.Symmetric
import Control.Exception                          hiding (bracket)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy.Char8                 qualified as BSL
import Data.Code.Generic
-- import Data.Map                                         (Map)
import Data.Map                                   qualified as M
import Data.MapSet
-- import Data.Maybe
import Data.Render.Library.Imports
import Data.Render.Library.Longhand
import Data.Render.Library.Shorthand
import Data.Render.Program.Imports
import Data.Render.Program.Longhand
import Data.Render.Program.Shorthand
import Data.Render.Statement.Longhand
import Data.Render.Statement.Shorthand
-- import Data.Set                                         (Set)
import Data.Set                                   qualified as S
-- import Data.String
-- import Data.Typeable
import GHC.IO.Exception
import GHC.IsList
import Prelude                                    hiding (id, (.))
import System.Process
import Text.Read

newtype JS a b = JS {
    _code :: Code a b
} deriving stock (Eq, Show)

instance HasCode JS a b where
    code = _code

toExternalCLIImports ∷ JS a b → [String]
toExternalCLIImports js = GHC.IsList.toList (externalImports js) >>=
    \(moduleName, functions) -> ["import { " <> BSL.unpack (BSL.intercalate ", " (S.toList functions)) <> " } from \"" <> BSL.unpack moduleName <> "\";"]

toInternalCLIImports ∷ JS a b → [String]
toInternalCLIImports js = GHC.IsList.toList (internalImports js) >>=
    \(moduleName, functions) -> ["import { " <> BSL.unpack (BSL.intercalate ", " (functionName <$> S.toList functions)) <> " } from \"" <> BSL.unpack moduleName <> "\";"]

toShorthandCLIDefinitions ∷ JS a b → [String]
toShorthandCLIDefinitions js = GHC.IsList.toList (internalImports js) >>=
    \(_, functions) -> GHC.IsList.toList functions >>=
    \function' -> [
        BSL.unpack $
            "const " <> functionName function' <> " = " <> functionLonghand function' <> ";"
        ]

toInternalFileImports ∷ JS a b → [BSL.ByteString]
toInternalFileImports js = (
    \(moduleName, functions) ->
        "import { " <> BSL.intercalate ", " (functionName <$> S.toList functions) <> " } from \"" <> moduleName <> "\";"
    ) <$> M.toList (getMapSet (internalImports js))

toShorthandFileDefinitions ∷ JS a b → [BSL.ByteString]
toShorthandFileDefinitions js = foldMap (\(_, functions) ->
    foldMap (\fn ->
        [
            "/**\n * @param {" <> functionTypeFrom fn <> "} param\n * @returns {" <> functionTypeTo fn <> "}\n */\n" <>
            "export const " <> functionName fn <> " = " <> functionLonghand fn <> "\n"
        ]
    )
    functions
    ) $ M.toList (getMapSet (internalImports js))

toExternalFileImports ∷ JS a b → [BSL.ByteString]
toExternalFileImports js = (
    \(moduleName, functions) ->
        "import " <> BSL.intercalate ", " (S.toList functions) <> " from \"" <> moduleName <> "\";"
    ) <$> M.toList (getMapSet (externalImports js))

instance RenderStatementLonghand (JS a b) where
    renderStatementLonghand = longhand

instance RenderStatementShorthand (JS a b) where
    renderStatementShorthand = shorthand

instance RenderLibraryShorthand (JS a b) where
    renderLibraryShorthand _ = []

instance RenderLibraryLonghand (JS a b) where
    renderLibraryLonghand _ = []

instance RenderLibraryImports (JS a b) where
    renderLibraryImports _ = []

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramShorthand (JS a b) where
    renderProgramShorthand cat =
        -- "\nmodule " <> module' cat <> " (" <> functionName cat <> ")  where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (toShorthandFileDefinitions cat) <>
        -- "\n" <> functionName cat <> " :: " <> functionTypeFrom cat <> " -> " <> functionTypeTo cat <> --  <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> functionName cat <> " = " <> renderStatementShorthand cat
        "\n" <> renderStatementShorthand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramLonghand (JS a b) where
    renderProgramLonghand cat =
        BSL.unlines (toExternalFileImports cat) <>
        -- "\n" <> functionName cat <> " :: " <> functionTypeFrom cat <> " -> " <> functionTypeTo cat <> -- BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> functionName cat <> " = " <> renderStatementLonghand cat
        "\n" <> renderStatementLonghand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -}  RenderProgramImports (JS a b) where
    renderProgramImports cat =
       BSL.unlines (toExternalFileImports cat) <>
       BSL.unlines (toInternalFileImports cat) <>
       "\n" <> renderStatementShorthand cat

{-}
instance Bracket JS where
    bracket js@(JS s) = JS $ s {
        _shorthand = "(" <> renderStatementShorthand js <> ")",
        _longhand = "(" <> renderStatementLonghand js <> ")"
    }
-}

instance Category JS where
    id = JS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "control/category", [
                    Function {
                        _functionName = "id",
                        _functionTypeFrom = "a",
                        _functionTypeTo = "a",
                        _functionShorthand = "a => a",
                        _functionLonghand = "a => a"
                    }
                ]
            )
        ],
        _shorthand = "id",
        _longhand = "a => a"
    }
    a . b = JS $ Code {
        _externalImports = externalImports a <> externalImports b,
        _internalImports = internalImports a <> internalImports b <> [
            ("control/category", [
                Function {
                    _functionName = "compose",
                    _functionTypeFrom = "TODO",
                    _functionTypeTo = "TODO",
                    _functionShorthand = "f => g => x => f(g(x))",
                    _functionLonghand = "f => g => x => f(g(x))"
                }
            ])
        ],
        _shorthand = "compose(" <> shorthand a <> ")(" <> shorthand b <> ")",
        _longhand = "(f => g => x => f(g(x)))(" <> longhand a <> ")(" <> longhand b <> ")"
    }

instance Cartesian JS where
    copy = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cartesian", [
                Function {
                    _functionName = "copy",
                    _functionTypeFrom = "a",
                    _functionTypeTo = "[a]",
                    _functionShorthand = "x => ([x, x])",
                    _functionLonghand = "x => ([x, x])"
                }
                ]
            )
        ],
        _shorthand = "copy",
        _longhand = "x => ([x, x])"
    }
    consume = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cartesian", [
                Function {
                    _functionName = "consume",
                    _functionTypeFrom = "a",
                    _functionTypeTo = "null",
                    _functionShorthand = "x => null",
                    _functionLonghand = "x => null"
                }
                ]
            )
        ],
        _shorthand = "consume",
        _longhand = "x => null"
    }
    fst' = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cartesian", [
                Function {
                    _functionName = "fst",
                    _functionTypeFrom = "[a, b]",
                    _functionTypeTo = "a",
                    _functionShorthand = "([a, b]) => a",
                    _functionLonghand = "([a, b]) => a"
                }
                ]
            )
        ],
        _shorthand = "fst",
        _longhand = "([a, b]) => a"
    }
    snd' = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cartesian", [
                Function {
                    _functionName = "snd",
                    _functionTypeFrom = "[a, b]",
                    _functionTypeTo = "b",
                    _functionShorthand = "([a, b]) => b",
                    _functionLonghand = "([a, b]) => b"
                }
                ]
            )
        ],
        _shorthand = "snd",
        _longhand = "([a, b]) => b"
    }

instance Cocartesian JS where
    injectL = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cocartesian", [
                Function {
                    _functionName = "injectL",
                    _functionTypeFrom = "a",
                    _functionTypeTo = "{ Left: a } | { Right: a }",
                    _functionShorthand = "a => ({ Left: a })",
                    _functionLonghand = "a => ({ Left: a })"
                }
                ]
            )
        ],
        _shorthand = "injectL",
        _longhand = "a => ({ Left: a })"
    }
    injectR = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cocartesian", [
                Function {
                    _functionName = "injectR",
                    _functionTypeFrom = "a",
                    _functionTypeTo = "{ Left: a } | { Right: a }",
                    _functionShorthand = "a => ({ Right: a })",
                    _functionLonghand = "a => ({ Right: a })"
                }
                ]
            )
        ],
        _shorthand = "injectR",
        _longhand = "a => ({ Right: a })"
    }
    unify = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cocartesian", [
                Function {
                    _functionName = "unify",
                    _functionTypeFrom = "{ Left: a } | { Right: a }",
                    _functionTypeTo = "a",
                    _functionShorthand = "x => x.Right || x.Left",
                    _functionLonghand = "x => x.Right || x.Left"
                }
                ]
            )
        ],
        _shorthand = "unify",
        _longhand = "x => x.Right ?? x.Left"
    }
    tag = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cocartesian", [
                Function {
                    _functionName = "tag",
                    _functionTypeFrom = "[boolean, a]",
                    _functionTypeTo = "{ Left: a } | { Right: a }",
                    _functionShorthand = "([tf, a]) => ({[tf ? \"Right\" : \"Left\"]: a})",
                    _functionLonghand = "([tf, a]) => ({[tf ? \"Right\" : \"Left\"]: a})"
                }
                ]
            )
        ],
        _shorthand = "tag",
        _longhand = "([tf, a]) => ({[tf ? \"Right\" : \"Left\"]: a})"
    }

-- >>> import Control.Category
-- >>> ((Control.Category..) fst' copy) :: JS String String
-- JS {_code = Code {_externalImports = MapSet {getMapSet = fromList []}, _internalImports = MapSet {getMapSet = fromList [("Control.Category.Cartesian",fromList [Function {_functionName = "copy", _functionTypeFrom = "a", _functionTypeTo = "(a, a)", _shorthand = "\\x -> (x, x)", _longhand = "\\x -> (x, x)"},Function {_functionName = "fst", _functionTypeFrom = "(a, b)", _functionTypeTo = "a", _shorthand = "fst", _longhand = "\\(a, b) -> a"}])]}, _module = "Control.Category.Function", _function = Function {_functionName = "(.)", _functionTypeFrom = "(a -> (a, a)) -> ((a, b) -> a)", _functionTypeTo = "a -> a", _shorthand = "(fst . \\x -> (x, x))", _longhand = "(\\(a, b) -> a . \\x -> (x, x))"}}}

-- >>> renderStatementLonghand (((Control.Category..) fst' copy) :: JS String String)

instance Strong JS where
    first' f = JS $ Code {
        _externalImports = externalImports f,
        _internalImports = internalImports f,
        _shorthand = "([a, b]) => ([(" <> shorthand f <> ")(a), b])",
        _longhand = "([a, b]) => ([(" <> longhand f <> ")(a), b])"
    }
    second' f = JS $ Code {
        _externalImports = externalImports f,
        _internalImports = internalImports f,
        _shorthand = "([a, b]) => ([a, (" <> longhand f <> ")(b)])",
        _longhand = "([a, b]) => ([a, (" <> longhand f <> ")(b)])"
    }

instance Choice JS where
    left' f = JS $ Code {
        _externalImports = externalImports f,
        _internalImports = internalImports f,
        _shorthand = "x => x.Left && ({ Left: (" <> shorthand f <> ")(x.Left) }) || x",
        _longhand = "x => x.Left && ({ Left: (" <> longhand f <> ")(x.Left) }) || x"
    }
    right' f = JS $ Code {
        _externalImports = externalImports f,
        _internalImports = internalImports f,
        _shorthand = "x => x.Right && ({ Right: (" <> shorthand f <> ")(x.Right) }) || x",
        _longhand = "x => x.Right && ({ Right: (" <> longhand f <> ")(x.Right) }) || x"
    }
instance Symmetric JS where
    swap = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/symmetric", [
                Function {
                    _functionName = "swap",
                    _functionTypeFrom = "[a, b]",
                    _functionTypeTo = "[b, a]",
                    _functionShorthand = "([a, b]) => ([b, a])",
                    _functionLonghand = "([a, b]) => ([b, a])"
                }
                ]
            )
            ],
        _shorthand = "swap",
        _longhand = "([a, b]) => ([b, a])"
    }
    swapEither = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/symmetric", [
                Function {
                    _functionName = "swapEither",
                    _functionTypeFrom = "{ Left : x }  | { Right : x }",
                    _functionTypeTo = "{ Left : x }  | { Right : x }",
                    _functionShorthand = "x => x?.Left && ({ Right: x.Left }) || ({ Left: x.Right })",
                    _functionLonghand = "x => x?.Left && ({ Right: x.Left }) || ({ Left: x.Right })"
                }
                ]
            )
        ],
        _shorthand = "swapEither",
        _longhand = "x => x?.Left && ({ Right: x.Left }) || ({ Left: x.Right })"
    }
    reassoc = JS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "control/category/symmetric",
                [
                    Function {
                        _functionName = "reassoc",
                        _functionTypeFrom = "[a, [b, c]]",
                        _functionTypeTo = "[[a, b], c]",
                        _functionShorthand = "([a, [b, c]]) => ([[a, b], c])",
                        _functionLonghand = "([a, [b, c]]) => ([[a, b], c])"
                    }
                    ]
            )
            ],
        _shorthand = "reassoc",
        _longhand = "([a, [b, c]]) => ([[a, b], c])"
    }
    reassocEither = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/symmetric", [
                Function {
                    _functionName = "reassocEither",
                    _functionTypeFrom = "TODO",
                    _functionTypeTo = "TODO",
                    -- \\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c }
                    _functionShorthand = "x => { throw new Error(\"TODO: reassocEither\"); }",
                    _functionLonghand = "x => { throw new Error(\"TODO: reassocEither\"); }"
                }
                ]
            )
            ],
        _shorthand = "reassocEither",
        _longhand = "TODO"
    }

-- instance Cochoice JS where

-- instance Costrong JS where

-- instance Apply JS where

instance PrimitiveBool JS where
    eq = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/bool", [
                Function {
                    _functionName = "eq",
                    _functionTypeFrom = "[a, a]",
                    _functionTypeTo = "boolean",
                    _functionShorthand = "([x, y]) => x === y",
                    _functionLonghand = "([x, y]) => x === y"
                }
            ])
        ],
        _shorthand = "eq",
        _longhand = "([x, y]) => x === y"
    }

instance PrimitiveConsole JS where
    outputString = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/console", [
                Function {
                    _functionName = "outputString",
                    _functionTypeFrom = "string",
                    _functionTypeTo = "void",
                    _functionShorthand = "console.log",
                    _functionLonghand = "console.log"
                }
            ])
        ],
        _shorthand = "outputString",
        _longhand = "console.log"
    }
    inputString = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/console", [
                Function {
                    _functionName = "inputString",
                    _functionTypeFrom = "void",
                    _functionTypeTo = "string",
                    _functionShorthand = "x => { throw new Error(\"TODO Node or browser?\"); }",
                    _functionLonghand = "x => { throw new Error(\"TODO Node or browser?\"); }"
                }
            ])
        ],
        _shorthand = "inputString",
        _longhand = "TODO Node or browser?"
    }

instance PrimitiveExtra JS where
    intToString = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/extra", [
                Function {
                    _functionName = "intToString",
                    _functionTypeFrom = "number",
                    _functionTypeTo = "string",
                    _functionShorthand = "String",
                    _functionLonghand = "String"
                }
                ]
            )
            ],
        _shorthand = "intToString",
        _longhand = "String"
    }
    concatString = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/extra", [
                Function {
                    _functionName = "concatString",
                    _functionTypeFrom = "[string, string]",
                    _functionTypeTo = "string",
                    _functionShorthand = "([a, b]) => a + b",
                    _functionLonghand = "([a, b]) => a + b"
                }
                ]
            )
            ],
        _shorthand = "concatString",
        _longhand = "([a, b]) => a + b"
    }
    constString s = JS $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "_x => \"" <> BSL.pack s <> "\"",
        _longhand = "_x => \"" <> BSL.pack s <> "\""
    }

{-
instance PrimitiveFile JS where
    readFile' = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Primitive.File", [
                Function {
                    _functionName = "readFile'",
                    _functionTypeFrom = "String",
                    _functionTypeTo = "IO String",
                    _functionShorthand = "(Kleisli $ liftIO . readFile)",
                    _functionLonghand = "(Kleisli $ liftIO . readFile)"
                }
                ]
            )
            ],
        _shorthand = "readFile'",
        _longhand = "(Kleisli $ liftIO . readFile)"
    }
    writeFile' = JS $ Code {
        _externalImports = [
            ("Control.Arrow", ["Kleisli(..)"]),
            ("Control.Category", ["(.)"]),
            ("Control.Monad.IO.Class", ["liftIO"])
        ],
        _internalImports = [
            ("Control.Category.Primitive.File", [
                Function {
                    _functionName = "writeFile'",
                    _functionTypeFrom = "(String, String)",
                    _functionTypeTo = "IO ()",
                    _functionShorthand = "(Kleisli $ liftIO . uncurry writeFile)",
                    _functionLonghand = "(Kleisli $ liftIO . uncurry writeFile)"
                }
                ]
            )
            ],
        _shorthand = "writeFile'",
        _longhand = "(Kleisli $ liftIO . uncurry writeFile)"
    }
-}

instance PrimitiveString JS where
    reverseString = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/primitive/string", [
                Function {
                    _functionName = "reverseString",
                    _functionTypeFrom = "string",
                    _functionTypeTo = "string",
                    _functionShorthand = "x => x.split('').reverse().join('')",
                    _functionLonghand = "x => x.split('').reverse().join('')"
                }
                ]
            )
            ],
        _shorthand = "reverseString",
        _longhand = "x => x.split('').reverse().join('')"
    }

instance Numeric JS where
    num n = JS $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "_x => " <> BSL.pack (show n),
        _longhand = "_x => " <> BSL.pack (show n)
    }
    negate' = JS $ Code {
        _externalImports = [],
        _internalImports = [(
            "control/category/numeric", [
                Function {
                    _functionName = "negate",
                    _functionTypeFrom = "number",
                    _functionTypeTo = "number",
                    _functionShorthand = "x => -x",
                    _functionLonghand = "x => -x"
                }
            ]
        )],
        _shorthand = "negate",
        _longhand = "x => -x"
    }
    add = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/numeric",  [
                Function {
                    _functionName = "add",
                    _functionTypeFrom = "[number, number]",
                    _functionTypeTo = "number",
                    _functionShorthand = "([x, y]) => x + y",
                    _functionLonghand = "([x, y]) => x + y"
                }
                ]
            )
            ],
        _shorthand = "add",
        _longhand = "([x, y]) => x + y"
    }
    mult = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/numeric",  [
                Function {
                    _functionName = "mult",
                    _functionTypeFrom = "[number, number]",
                    _functionTypeTo = "number",
                    _functionShorthand = "([x, y]) => x * y",
                    _functionLonghand = "([x, y]) => x * y"
                }
                ]
            )
            ],
        _shorthand = "mult",
        _longhand = "([x, y]) => x * y"
    }
    div' = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/numeric",  [
                Function {
                    _functionName = "div",
                    _functionTypeFrom = "[number, number]",
                    _functionTypeTo = "number",
                    _functionShorthand = "([x, y]) => Math.floor(x / y)",
                    _functionLonghand = "([x, y]) => Math.floor(x / y)"
                }
                ]
            )
            ],
        _shorthand = "div",
        _longhand = "([x, y]) => Math.floor(x / y)"
    }
    mod' = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/numeric",  [
                Function {
                    _functionName = "mod",
                    _functionTypeFrom = "[number, number]",
                    _functionTypeTo = "number",
                    _functionShorthand = "([x, y]) => x % y",
                    _functionLonghand = "([x, y]) => x % y"
                }
                ]
            )
            ],
        _shorthand = "mod",
        _longhand = "([x, y]) => x % y"
    }

{-}
-- I don't quite know how to call node or cabal repl to include the correct functions here, so the tests are skipped.

-- @TODO escape shell - Text.ShellEscape?
-}

-- instance ExecuteJSLonghand JS where
--     executeJSLonghand cat param = do
--         let params ∷ [String]
--             params = [
--                 "-e",
--                 unwords (toExternalCLIImports cat) <>
--                     "(" <> BSL.unpack (renderStatementLonghand cat) <> ") (" <> param <> ")"
--                 ]
--         (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
--         case exitCode of
--             ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
--             ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)
-- 
-- instance ExecuteJSShorthand JS where
--     executeJSShorthand cat param = do
--         let params ∷ [String]
--             params = [
--                 "-e",
--                 unwords (toExternalCLIImports cat) <>
--                     unwords (toShorthandCLIDefinitions cat) <>
--                     "'(" <> BSL.unpack (renderStatementShorthand cat) <> ") (" <> show param <> ")"
--                 ]
--         (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
--         case exitCode of
--             ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
--             ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)
-- 
-- instance ExecuteJSImports JS where
--     executeJSImports cat param = do
--         let params ∷ [String]
--             params = [
--                 "-e",
--                 unwords (toExternalCLIImports cat) <>
--                     unwords (toInternalCLIImports cat) <>
--                     "'(" <> BSL.unpack (renderStatementShorthand cat) <> ") (" <> show param <> ")"
--                 ]
--         (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
--         case exitCode of
--             ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
--             ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

-- @TODO this passes too many arguments apparently...
-- This is because of the id and (.) using the (->) instance whereas I am running Kleisli below.
-- This means we need to deal with both within JS sessions. Let's try to use Pure/Monadic... or maybe JSPure / JSMonadic accepting only appropriate typeclasses / primitives?
instance ExecuteStdioLonghand JS where
    executeStdioLonghand cat stdin = do
        let params ∷ [String]
            params = [
                "-e",
                unwords (toExternalCLIImports cat) <>
                    "(" <> BSL.unpack (renderStatementLonghand cat) <> ")(null)"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

instance ExecuteStdioImports JS where
    executeStdioImports cat stdin = do
        let params ∷ [String]
            params = [
                "-e",
                -- toCLIImports cat <>
                -- [
                    "(" <> BSL.unpack (renderStatementShorthand cat) <> ")(null)"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

instance ExecuteStdioShorthand JS where
    executeStdioShorthand cat stdin = do
        let params ∷ [String]
            params = [
                "-e",
                unwords (toExternalCLIImports cat) <>
                    "(" <> BSL.unpack (renderStatementShorthand cat) <> ")(null)"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params (show stdin))
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (readEither stdout)

instance ExecuteJSONLonghand JS where
    executeJSONLonghand cat param = do
        let params ∷ [String]
            params = [
                "-e",
                unwords (toExternalCLIImports cat) <>
                    "console.log(JSON.stringify((" <> BSL.unpack (renderStatementLonghand cat) <> ")(JSON.parse(" <> show (BSL.unpack (encode param)) <> "))))"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (eitherDecode (BSL.pack stdout))

instance ExecuteJSONShorthand JS where
    executeJSONShorthand :: (ToJSON input, FromJSON output, MonadIO m) => JS input output -> input -> m output
    executeJSONShorthand cat param = do
        let params ∷ [String]
            params = [
                "-e",
                unwords (toExternalCLIImports cat) <>
                    unwords (toShorthandCLIDefinitions cat) <>
                    "console.log(JSON.stringify((" <> BSL.unpack (renderStatementShorthand cat) <> ")(JSON.parse(" <> show (BSL.unpack (encode param)) <> "))))"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (eitherDecode (BSL.pack stdout))

instance ExecuteJSONImports JS where
    executeJSONImports cat param = do
        let params ∷ [String]
            params = [
                    "-e",
                    unwords (toExternalCLIImports cat) <>
                        unwords (toInternalCLIImports cat) <>
                        "console.log(JSON.stringify((" <> BSL.unpack (renderStatementShorthand cat) <> ")(JSON.parse(" <> show (BSL.unpack (encode param)) <> "))))"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "node" params "")
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run node with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . (\ex -> "Can't parse response: " <> ex <> ", params = " <> unwords params <> ", stdout = " <> stdout <> ", stderr = " <> stderr)) pure (eitherDecode (BSL.pack stdout))
