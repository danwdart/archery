{- GeneralisedNewtypeDeriving -}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe               #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

-- | Program module. Like Func, but dynamically imports modules as required.
module Data.Code.PHP where

import Control.Category
-- import Control.Category.Apply
-- import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
-- import Control.Category.Execute.PHP.Imports
-- import Control.Category.Execute.PHP.Longhand
-- import Control.Category.Execute.PHP.Shorthand
-- import Control.Category.Execute.JSON.Imports
-- import Control.Category.Execute.JSON.Longhand
-- import Control.Category.Execute.JSON.Shorthand
-- import Control.Category.Execute.Stdio.Imports
-- import Control.Category.Execute.Stdio.Longhand
-- import Control.Category.Execute.Stdio.Shorthand
-- import Control.Category.Numeric
import Control.Category.Primitive.Bool
-- import Control.Category.Primitive.Console
-- -- import Control.Category.Primitive.Extra
-- -- import Control.Category.Primitive.File
-- -- import Control.Category.Primitive.String
import Control.Category.Strong
import Control.Category.Symmetric
-- import Control.Exception                          hiding (bracket)
-- import Control.Monad.IO.Class
-- import Data.Aeson
import Data.ByteString.Lazy.Char8                 qualified as BSL
import Data.Code.Generic
-- import Data.Map                                         (Map)
import Data.Map                                   qualified as M
import Data.MapSet
-- import Data.Maybe
import Data.Render.Program.Imports
import Data.Render.Program.Longhand
import Data.Render.Program.Shorthand
import Data.Render.Statement.Longhand
import Data.Render.Statement.Shorthand
-- import Data.Set                                         (Set)
-- import Data.Set                                   qualified as S
-- import Data.String
-- import Data.Typeable
-- import GHC.IO.Exception
import GHC.IsList
import Prelude                                    hiding (id, (.))
-- import System.Process
-- import Text.Read

newtype PHP a b = PHP {
    _code :: Code a b
} deriving stock (Eq, Show)

instance HasCode PHP a b where
    code = _code

toExternalCLIImports ∷ PHP a b → [String]
toExternalCLIImports php = GHC.IsList.toList (externalImports php) >>=
    \(moduleName, functions) -> GHC.IsList.toList functions >>=
        \functionName' -> [BSL.unpack $ "use function " <> moduleName <> "\\" <> functionName' <> ";"]

toInternalCLIImports ∷ PHP a b → [String]
toInternalCLIImports php = GHC.IsList.toList (externalImports php) >>=
    \(moduleName, functions) -> GHC.IsList.toList functions >>=
        \functionName' -> [BSL.unpack $ "use function " <> moduleName <> "\\" <> functionName' <> ";"]

toShorthandCLIDefinitions ∷ PHP a b → [String]
toShorthandCLIDefinitions php = GHC.IsList.toList (internalImports php) >>=
    \(_, functions) -> GHC.IsList.toList functions >>=
    \function' -> [
        BSL.unpack $
            -- Why not both?
            "$" <> functionName function' <> " = " <> functionLonghand function' <> ";" <>
            "function " <> functionName function' <> "($param) { return (" <> functionLonghand function' <> ")($param); } " -- spacey
        ]

toInternalFileImports ∷ PHP a b → [BSL.ByteString]
toInternalFileImports php = GHC.IsList.toList (internalImports php) >>=
    \(moduleName, functions) -> GHC.IsList.toList functions >>=
        \function' -> ["use function " <> moduleName <> "\\" <> functionName function' <> ";"]

toShorthandFileDefinitions ∷ PHP a b → [BSL.ByteString]
toShorthandFileDefinitions php = foldMap (\(_, functions) ->
    foldMap (\fn ->
        [
            -- again, why not both?
            "$" <> functionName fn <> " = " <> functionLonghand fn <> "\n" <>
            "function " <> functionName fn <> "($param) { return (" <> functionLonghand fn <> ")($param); }\n" -- spacey
        ]
    )
    functions
    ) $ M.toList (getMapSet (internalImports php))

toExternalFileImports ∷ PHP a b → [BSL.ByteString]
toExternalFileImports php = GHC.IsList.toList (externalImports php) >>=
    \(moduleName, functions) -> GHC.IsList.toList functions >>=
        \functionName' -> ["use function " <> moduleName <> "\\" <> functionName' <> ";"]

instance RenderStatementLonghand (PHP a b) where
    renderStatementLonghand = longhand

instance RenderStatementShorthand (PHP a b) where
    renderStatementShorthand = shorthand

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramShorthand (PHP a b) where
    renderProgramShorthand cat =
        -- "\nmodule " <> module' cat <> " (" <> functionName cat <> ")  where\n\n" <>
        BSL.unlines (toExternalFileImports cat) <>
        BSL.unlines (toShorthandFileDefinitions cat) <>
        -- "\n" <> functionName cat <> " :: " <> functionTypeFrom cat <> " -> " <> functionTypeTo cat <> --  <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> renderStatementShorthand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderProgramLonghand (PHP a b) where
    renderProgramLonghand cat =
        BSL.unlines (toExternalFileImports cat) <>
        -- "\n" <> functionName cat <> " :: " <> functionTypeFrom cat <> " -> " <> functionTypeTo cat <> -- BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        "\n" <> renderStatementLonghand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -}  RenderProgramImports (PHP a b) where
    renderProgramImports cat =
       BSL.unlines (toExternalFileImports cat) <>
       BSL.unlines (toInternalFileImports cat) <>
       "\n" <> renderStatementShorthand cat

{-}
instance Bracket PHP where
    bracket php@(PHP s) = PHP $ s {
        _shorthand = "(" <> renderStatementShorthand php <> ")",
        _longhand = "(" <> renderStatementLonghand php <> ")"
    }
-}

instance Category PHP where
    id = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "Control\\Category", [
                    Function {
                        _functionName = "id",
                        _functionTypeFrom = "",
                        _functionTypeTo = "",
                        _functionShorthand = "fn($a) => $a",
                        _functionLonghand = "fn($a) => $a"
                    }
                ]
            )
        ],
        _shorthand = "$id",
        _longhand = "fn($a) => $a"
    }
    a . b = PHP $ Code {
        _externalImports = externalImports a <> externalImports b,
        _internalImports = internalImports a <> internalImports b <> [
            ("Control\\Category", [
                Function {
                    _functionName = "compose",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn($f) => fn($g) => fn($x) => $f($g($x))",
                    _functionLonghand = "fn($f) => fn($g) => fn($x) => $f($g($x))"
                }
            ])
        ],
        _shorthand = "$compose(" <> shorthand a <> ")(" <> shorthand b <> ")",
        _longhand = "(fn ($f) => fn ($g) => fn($x) => $f($g($x)))(" <> longhand a <> ")(" <> longhand b <> ")"
    }

instance Cartesian PHP where
    copy = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cartesian", [
                Function {
                    _functionName = "copy",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => ([$x, $x])",
                    _functionLonghand = "fn ($x) => ([$x, $x])"
                }
                ]
            )
        ],
        _shorthand = "$copy",
        _longhand = "fn ($x) => ([$x, $x])"
    }
    consume = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cartesian", [
                Function {
                    _functionName = "consume",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => null",
                    _functionLonghand = "fn ($x) => null"
                }
                ]
            )
        ],
        _shorthand = "$consume",
        _longhand = "fn ($x) => null"
    }
    fst' = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cartesian", [
                Function {
                    _functionName = "fst",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => $x[0]",
                    _functionLonghand = "fn ($x) => $x[0]"
                }
                ]
            )
        ],
        _shorthand = "$fst",
        _longhand = "fn ($x) => $x[0]"
    }
    snd' = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cartesian", [
                Function {
                    _functionName = "snd",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => $x[1]",
                    _functionLonghand = "fn ($x) => $x[1]"
                }
                ]
            )
        ],
        _shorthand = "$snd",
        _longhand = "fn ($x) => $x[1]"
    }

instance Cocartesian PHP where
    injectL = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cocartesian", [
                Function {
                    _functionName = "injectL",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn($x) => [\"Left\" => $a]",
                    _functionLonghand = "fn($x) => [\"Left\" => $a]"
                }
                ]
            )
        ],
        _shorthand = "$injectL",
        _longhand = "fn($x) => [ \"Left\" => $ a })"
    }
    injectR = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cocartesian", [
                Function {
                    _functionName = "injectR",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn($x) => [\"Right\" => $a]",
                    _functionLonghand = "fn($x) => [\"Right\" => $a]"
                }
                ]
            )
        ],
        _shorthand = "$injectR",
        _longhand = "fn($x) => [\"Right\" => $a]"
    }
    unify = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cocartesian", [
                Function {
                    _functionName = "unify",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn($x) => isset($x[\"Right\"]) ? $x[\"Right\"] : $x[\"Left\"]",
                    _functionLonghand = "fn($x) => isset($x[\"Right\"]) ? $x[\"Right\"] : $x[\"Left\"]"
                }
                ]
            )
        ],
        _shorthand = "$unify",
        _longhand = "fn($x) => isset($x[\"Right\"]) ? $x[\"Right\"] : $x[\"Left\"]"
    }
    tag = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Cocartesian", [
                Function {
                    _functionName = "tag",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn($x) => $x[0] ? [\"Right\" => $x[1]] : [\"Left\" => $x[1]]",
                    _functionLonghand = "fn($x) => $x[0] ? [\"Right\" => $x[1]] : [\"Left\" => $x[1]]"
                }
                ]
            )
        ],
        _shorthand = "$tag",
        _longhand = "fn($x) => $x[0] ? [\"Right\" => $x[1]] : [\"Left\" => $x[1]]"
    }

-- >>> import Control.Category
-- >>> ((Control.Category..) fst' copy) :: PHP String String
-- PHP {_code = Code {_externalImports = MapSet {getMapSet = fromList []}, _internalImports = MapSet {getMapSet = fromList [("Control.Category.Cartesian",fromList [Function {_functionName = "copy", _functionTypeFrom = "a", _functionTypeTo = "(a, a)", _shorthand = "\\x -> (x, x)", _longhand = "\\x -> (x, x)"},Function {_functionName = "fst", _functionTypeFrom = "(a, b)", _functionTypeTo = "a", _shorthand = "fst", _longhand = "\\(a, b) -> a"}])]}, _module = "Control.Category.Function", _function = Function {_functionName = "(.)", _functionTypeFrom = "(a -> (a, a)) -> ((a, b) -> a)", _functionTypeTo = "a -> a", _shorthand = "(fst . \\x -> (x, x))", _longhand = "(\\(a, b) -> a . \\x -> (x, x))"}}}

-- >>> renderStatementLonghand (((Control.Category..) fst' copy) :: PHP String String)

instance Strong PHP where
    first' f = PHP $ Code {
        _externalImports = externalImports f,
        _internalImports = internalImports f,
        _shorthand = "fn ($x) => [(" <> shorthand f <> ")($x[0]), $x[1]]",
        _longhand = "fn ($x) => [(" <> longhand f <> ")($x[0]), $x[1]]"
    }
    second' f = PHP $ Code {
        _externalImports = externalImports f,
        _internalImports = internalImports f,
        _shorthand = "fn ($x) => [$x[0], (" <> shorthand f <> ")($x[1])]",
        _longhand = "fn ($x) => [$x[0], (" <> shorthand f <> ")($x[1])]"
    }

instance Choice PHP where
    left' f = PHP $ Code {
        _externalImports = externalImports f,
        _internalImports = internalImports f,
        _shorthand = "fn($x) => isset($x[\"Left\"] ? [ \"Left\" => (" <> shorthand f <> ")($x[\"Left\"]) ] : $x",
        _longhand = "fn($x) => isset($x[\"Left\"] ? [ \"Left\" => (" <> shorthand f <> ")($x[\"Left\"]) ] : $x"
    }
    right' f = PHP $ Code {
        _externalImports = externalImports f,
        _internalImports = internalImports f,
        _shorthand = "fn($x) => isset($x[\"Right\"] ? [ \"Right\" => (" <> shorthand f <> ")($x[\"Right\"]) ] : $x",
        _longhand = "fn($x) => isset($x[\"Right\"] ? [ \"Right\" => (" <> shorthand f <> ")($x[\"Right\"]) ] : $x"
    }
instance Symmetric PHP where
    swap = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Symmetric", [
                Function {
                    _functionName = "swap",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => [$x[1], $x[0]]",
                    _functionLonghand = "fn ($x) => [$x[1], $x[0]]"
                }
                ]
            )
            ],
        _shorthand = "$swap",
        _longhand = "fn ($x) => [$x[1], $x[0]]"
    }
    swapEither = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Symmetric", [
                Function {
                    _functionName = "swapEither",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => isset($x[\"Left\"]) ? [\"Right\" => $x[\"Left\"]] : [\"Left\" => $x[\"Right\"]]",
                    _functionLonghand = "fn ($x) => isset($x[\"Left\"]) ? [\"Right\" => $x[\"Left\"]] : [\"Left\" => $x[\"Right\"]]"
                }
                ]
            )
        ],
        _shorthand = "$swapEither",
        _longhand = "fn ($x) => isset($x[\"Left\"]) ? [\"Right\" => $x[\"Left\"]] : [\"Left\" => $x[\"Right\"]]"
    }
    reassoc = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "Control\\Category\\Symmetric",
                [
                    Function {
                        _functionName = "reassoc",
                        _functionTypeFrom = "",
                        _functionTypeTo = "",
                        _functionShorthand = "fn($x) => [[$x[0], $x[1][0]], $x[1][1]]",
                        _functionLonghand = "fn($x) => [[$x[0], $x[1][0]], $x[1][1]]"
                    }
                    ]
            )
            ],
        _shorthand = "$reassoc",
        _longhand = "fn($x) => [[$x[0], $x[1][0]], $x[1][1]]"
    }
    reassocEither = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Symmetric", [
                Function {
                    _functionName = "reassocEither",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    -- \\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c }
                    _functionShorthand = "fn($x) => { throw new Exception(\"TODO: reassocEither\"); }",
                    _functionLonghand = "fn($x) => { throw new Exception(\"TODO: reassocEither\"); }"
                }
                ]
            )
            ],
        _shorthand = "$reassocEither",
        _longhand = "fn($x) => { throw new Exception(\"TODO: reassocEither\"); }"
    }

-- instance Cochoice PHP where

-- instance Costrong PHP where

-- instance Apply PHP where

instance PrimitiveBool PHP where
    eq = PHP $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control\\Category\\Primitive\\Bool", [
                Function {
                    _functionName = "eq",
                    _functionTypeFrom = "",
                    _functionTypeTo = "",
                    _functionShorthand = "fn ($x) => $x[0] == $x[1]",
                    _functionLonghand = "fn ($x) => $x[0] == $x[1]"
                }
            ])
        ],
        _shorthand = "eq",
        _longhand = "fn ($x) => $x[0] == $x[1]"
    }

{-}
instance PrimitiveConsole PHP where
    outputString = PHP $ Code {
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
    inputString = PHP $ Code {
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

instance PrimitiveExtra PHP where
    intToString = PHP $ Code {
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
    concatString = PHP $ Code {
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
    constString s = PHP $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "_x => \"" <> BSL.pack s <> "\"",
        _longhand = "_x => \"" <> BSL.pack s <> "\""
    }

{-
instance PrimitiveFile PHP where
    readFile' = PHP $ Code {
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
    writeFile' = PHP $ Code {
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

instance PrimitiveString PHP where
    reverseString = PHP $ Code {
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

instance Numeric PHP where
    num n = PHP $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "_x => " <> BSL.pack (show n),
        _longhand = "_x => " <> BSL.pack (show n)
    }
    negate' = PHP $ Code {
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
    add = PHP $ Code {
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
    mult = PHP $ Code {
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
    div' = PHP $ Code {
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
    mod' = PHP $ Code {
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

-- instance ExecutePHPLonghand PHP where
--     executePHPLonghand cat param = do
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
-- instance ExecutePHPShorthand PHP where
--     executePHPShorthand cat param = do
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
-- instance ExecutePHPImports PHP where
--     executePHPImports cat param = do
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
-- This means we need to deal with both within PHP sessions. Let's try to use Pure/Monadic... or maybe PHPPure / PHPMonadic accepting only appropriate typeclasses / primitives?
instance ExecuteStdioLonghand PHP where
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

instance ExecuteStdioImports PHP where
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

instance ExecuteStdioShorthand PHP where
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

instance ExecuteJSONLonghand PHP where
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

instance ExecuteJSONShorthand PHP where
    executeJSONShorthand :: (ToJSON input, FromJSON output, MonadIO m) => PHP input output -> input -> m output
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

instance ExecuteJSONImports PHP where
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
-}