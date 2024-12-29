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
import Control.Category.Execute.JS.Imports
import Control.Category.Execute.JS.Longhand
import Control.Category.Execute.JS.Shorthand
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
import Control.Category.Primitive.File
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
import Data.Render.File.Imports
import Data.Render.File.Longhand
import Data.Render.File.Shorthand
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
    \(moduleName', _) -> [] -- {-}"-e", ":l", BSL.unpack moduleName,-} "-e", BSL.unpack $ "import " <> moduleName' {-<> " (" <> BSL.intercalate ", " (fst <$> S.toList imports') <> ")\""-}]

toInternalCLIImports ∷ JS a b → [String]
toInternalCLIImports js = GHC.IsList.toList (internalImports js) >>=
    \(moduleName', _) -> [] -- "-e", BSL.unpack $ "import " <> moduleName']

toShorthandCLIDefinitions ∷ JS a b → [String]
toShorthandCLIDefinitions js = [] {-} GHC.IsList.toList (internalImports js) >>=
    \(_, functions) -> GHC.IsList.toList functions >>=
    \function' -> [
        "-e", BSL.unpack $
            functionName function' <> " :: " <> functionTypeFrom function' <> " -> " <> functionTypeTo function' <> "; " <>
            functionName function' <> " = " <> functionLonghand function'
        ] -}

toInternalFileImports ∷ JS a b → [BSL.ByteString]
toInternalFileImports js = [] {- (
    \(moduleName, functions) ->
        "import " <> moduleName <> " (" <> BSL.intercalate ", " (functionName <$> S.toList functions) <> ")"
    ) <$> M.toList (getMapSet (internalImports js)) -}

toShorthandFileDefinitions ∷ JS a b → [BSL.ByteString]
toShorthandFileDefinitions js = [] {-foldMap (\(_, functions) ->
    foldMap (\fn ->
        [functionName fn <> " :: " <> functionTypeFrom fn <> " -> " <> functionTypeTo fn <> "\n" <>
            functionName fn <> " = " <> functionLonghand fn <> "\n"]
    )
    functions
    ) $ M.toList (getMapSet (internalImports js)) -}

toExternalFileImports ∷ JS a b → [BSL.ByteString]
toExternalFileImports js = [] {-(
    \(moduleName, functions) ->
        "import " <> moduleName <> " (" <> BSL.intercalate ", " (S.toList functions) <> ")"
    ) <$> M.toList (getMapSet (externalImports js)) -}

instance RenderStatementLonghand (JS a b) where
    renderStatementLonghand = longhand

instance RenderStatementShorthand (JS a b) where
    renderStatementShorthand = shorthand

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderFileShorthand (JS a b) where
    renderFileShorthand newModule' newFunctionName newFunctionTypeFrom newFunctionTypeTo cat = ""
        -- "\nmodule " <> module' cat <> " (" <> functionName cat <> ")  where\n\n" <>
        -- "\nmodule " <> newModule' <> " (" <> newFunctionName <> ") where\n\n" <>
        -- BSL.unlines (toExternalFileImports cat) <>
        -- BSL.unlines (toShorthandFileDefinitions cat) <>
        -- -- "\n" <> functionName cat <> " :: " <> functionTypeFrom cat <> " -> " <> functionTypeTo cat <> --  <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> newFunctionName <> " :: " <> newFunctionTypeFrom <> " -> " <> newFunctionTypeTo <> --  <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- -- "\n" <> functionName cat <> " = " <> renderStatementShorthand cat
        -- "\n" <> newFunctionName <> " = " <> renderStatementShorthand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -} RenderFileLonghand (JS a b) where
    renderFileLonghand newModule' newFunctionName newFunctionTypeFrom newFunctionTypeTo cat = ""
        -- "\nmodule " <> module' cat <> " (" <> functionName cat <> ")  where\n\n" <>
        -- "\nmodule " <> newModule' <> " (" <> newFunctionName <> ")  where\n\n" <>
        -- BSL.unlines (toExternalFileImports cat) <>
        -- -- "\n" <> functionName cat <> " :: " <> functionTypeFrom cat <> " -> " <> functionTypeTo cat <> -- BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> newFunctionName <> " :: " <> newFunctionTypeFrom <> " -> " <> newFunctionTypeTo <> -- BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- -- "\n" <> functionName cat <> " = " <> renderStatementLonghand cat
        -- "\n" <> newFunctionName <> " = " <> renderStatementLonghand cat

-- TODO runKleisli
instance {- (Typeable a, Typeable b) ⇒ -}  RenderFileImports (JS a b) where
    renderFileImports newModule' newFunctionName newFunctionTypeFrom newFunctionTypeTo cat = ""
        -- "\nmodule " <> module' cat <> " (" <> functionName cat <> ") where\n\n" <>
        -- "\nmodule " <> newModule' <> " (" <> newFunctionName <> ") where\n\n" <>
        -- BSL.unlines (toExternalFileImports cat) <>
        -- BSL.unlines (toInternalFileImports cat) <>
        -- -- "\n" <> functionName cat <> " :: " <> functionTypeFrom cat <> " -> " <> functionTypeTo cat <> -- <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- "\n" <> newFunctionName <> " :: " <> newFunctionTypeFrom  <> " -> " <> newFunctionTypeTo <> -- <> BSL.pack (showsTypeRep (mkFunTy (typeRep (Proxy :: Proxy a)) (typeRep (Proxy :: Proxy b))) "") <>
        -- -- "\n" <> functionName cat <> " = " <> renderStatementShorthand cat
        -- "\n" <> newFunctionName <> " = " <> renderStatementShorthand cat

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
                    _functionShorthand = "compose",
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
                    _functionShorthand = "copy",
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
            ("Control.Category.Cartesian", [
                Function {
                    _functionName = "consume",
                    _functionTypeFrom = "a",
                    _functionTypeTo = "null",
                    _functionShorthand = "consume",
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
                    _functionName = "fst'",
                    _functionTypeFrom = "[a, b]",
                    _functionTypeTo = "a",
                    _functionShorthand = "fst'",
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
            ("Control.Category.Cartesian", [
                Function {
                    _functionName = "snd'",
                    _functionTypeFrom = "[a, b]",
                    _functionTypeTo = "b",
                    _functionShorthand = "snd'",
                    _functionLonghand = "([a, b]) => b"
                }
                ]
            )
        ],
        _shorthand = "snd'",
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
                    _functionTypeTo = "{ tag: \"Left\" | \"Right\", val: a }",
                    _functionShorthand = "injectL",
                    _functionLonghand = "a => ({tag: \"Left\", val: a})"
                }
                ]
            )
        ],
        _shorthand = "injectL",
        _longhand = "a => ({tag: \"Left\", val: a})"
    }
    injectR = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Cocartesian", [
                Function {
                    _functionName = "injectR",
                    _functionTypeFrom = "a",
                    _functionTypeTo = "{ tag: \"Left\" | \"Right\", val: a }",
                    _functionShorthand = "injectR",
                    _functionLonghand = "a => ({tag: \"Right\", val: a})"
                }
                ]
            )
        ],
        _shorthand = "injectR",
        _longhand = "a => ({tag: \"Right\", val: a})"
    }
    unify = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("control/category/cocartesian", [
                Function {
                    _functionName = "unify",
                    _functionTypeFrom = "{ tag: \"Left\" | \"Right\", val: a }",
                    _functionTypeTo = "a",
                    _functionShorthand = "unify",
                    _functionLonghand = "({val}) => val"
                }
                ]
            )
        ],
        _shorthand = "unify",
        _longhand = "({val}) => val"
    }
    tag = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Cocartesian", [
                Function {
                    _functionName = "tag",
                    _functionTypeFrom = "[bool, a]",
                    _functionTypeTo = "{ tag: \"Left\" | \"Right\", val: a }",
                    _functionShorthand = "tag",
                    _functionLonghand = "([tf, a]) => ({tag: tf ? \"Right\" : \"Left\", val: a})"
                }
                ]
            )
        ],
        _shorthand = "tag",
        _longhand = "([tf, a]) => ({tag: tf ? \"Right\" : \"Left\", val: a})"
    }

-- >>> import Control.Category
-- >>> ((Control.Category..) fst' copy) :: JS String String
-- JS {_code = Code {_externalImports = MapSet {getMapSet = fromList []}, _internalImports = MapSet {getMapSet = fromList [("Control.Category.Cartesian",fromList [Function {_functionName = "copy", _functionTypeFrom = "a", _functionTypeTo = "(a, a)", _shorthand = "\\x -> (x, x)", _longhand = "\\x -> (x, x)"},Function {_functionName = "fst", _functionTypeFrom = "(a, b)", _functionTypeTo = "a", _shorthand = "fst", _longhand = "\\(a, b) -> a"}])]}, _module = "Control.Category.Function", _function = Function {_functionName = "(.)", _functionTypeFrom = "(a -> (a, a)) -> ((a, b) -> a)", _functionTypeTo = "a -> a", _shorthand = "(fst . \\x -> (x, x))", _longhand = "(\\(a, b) -> a . \\x -> (x, x))"}}}

-- >>> renderStatementLonghand (((Control.Category..) fst' copy) :: JS String String)

instance Strong JS where
    first' f = JS $ Code {
        _externalImports = externalImports f,
        _internalImports = internalImports f <> [
            ("control/category/strong", [
                Function {
                    _functionName = "first",
                    _functionTypeFrom = "TODO",
                    _functionTypeTo = "TODO",
                    _functionShorthand = "first (" <> shorthand f <> ")",
                    _functionLonghand = "([a, b]) => ([(" <> longhand f <> ")(a), b])"
                }
            ])
        ],
        _shorthand = "first (" <> shorthand f <> ")",
        _longhand = "([a, b]) => ([(" <> longhand f <> ")(a), b])"
    }
    second' f = JS $ Code {
        _externalImports = externalImports f,
        _internalImports = internalImports f <> [
            ("control/category/strong", [
                Function {
                    _functionName = "second",
                    _functionTypeFrom = "TODO",
                    _functionTypeTo = "TODO",
                    _functionShorthand = "second (" <> shorthand f <> ")",
                    _functionLonghand = "([a, b]) => ([a, (" <> longhand f <> ")(b)])"
                }
            ])
        ],
        _shorthand = "second (" <> shorthand f <> ")",
        _longhand = "\\(a, b) -> (a, (" <> longhand f <> ") b)"
    }

instance Choice JS where
    left' f = JS $ Code {
        _externalImports = externalImports f,
        _internalImports = internalImports f,
        _shorthand = "({tag, val}) => ({tag, val: tag === \"Left\" ? (" <> shorthand f <> ")(val) : val })",
        _longhand = "({tag, val}) => ({tag, val: tag === \"Left\" ? (" <> longhand f <> ")(val) : val })"
    }
    right' f = JS $ Code {
        _externalImports = externalImports f,
        _internalImports = internalImports f,
        _shorthand = "({tag, val}) => ({tag, val: tag === \"Right\" ? (" <> shorthand f <> ")(val) : val })",
        _longhand = "({tag, val}) => ({tag, val: tag === \"Right\" ? (" <> longhand f <> ")(val) : val })"
    }
{-
instance Symmetric JS where
    swap = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Symmetric", [
                Function {
                    _functionName = "swap",
                    _functionTypeFrom = "(a, b)",
                    _functionTypeTo = "(b, a)",
                    _functionShorthand = "\\(a, b) -> (b, a)",
                    _functionLonghand = "\\(a, b) -> (b, a)"
                }
                ]
            )
            ],
        _shorthand = "swap",
        _longhand = "\\(a, b) -> (b, a)"
    }
    swapEither = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Symmetric", [
                Function {
                    _functionName = "swapEither",
                    _functionTypeFrom = "Either a a",
                    _functionTypeTo = "Either a a",
                    _functionShorthand = "\\case { Left a -> Right a; Right a -> Left a; }",
                    _functionLonghand = "\\case { Left a -> Right a; Right a -> Left a; }"
                }
                ]
            )
        ],
        _shorthand = "swapEither",
        _longhand = "\\case { Left a -> Right a; Right a -> Left a; }"
    }
    reassoc = JS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "Control.Category.Symmetric",
                [
                    Function {
                        _functionName = "reassoc",
                        _functionTypeFrom = "(a, (b, c))",
                        _functionTypeTo = "((a, b), c)",
                        _functionShorthand = "\\(a, (b, c)) -> ((a, b), c)",
                        _functionLonghand = "\\(a, (b, c)) -> ((a, b), c)"
                    }
                    ]
            )
            ],
        _shorthand = "reassoc",
        _longhand = "\\(a, (b, c)) -> ((a, b), c)"
    }
    reassocEither = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Symmetric", [
                Function {
                    _functionName = "reassocEither",
                    _functionTypeFrom = "Either a (Either b c)",
                    _functionTypeTo = "Either (Either a b) c",
                    _functionShorthand = "\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c }",
                    _functionLonghand = "\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c }"
                }
                ]
            )
            ],
        _shorthand = "reassocEither",
        _longhand = "\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c }"
    }

-- instance Cochoice JS where

-- instance Costrong JS where

-- instance Apply JS where

instance PrimitiveBool JS where
    eq = JS $ Code {
        _externalImports = [
            ("Control.Arrow", ["arr"]),
            ("Control.Category", ["(.)"])
        ],
        _internalImports = [
            ("Control.Category.Primitive.Bool", [
                Function {
                    _functionName = "eq",
                    _functionTypeFrom = "Eq a => (a, a)",
                    _functionTypeTo = "Bool",
                    _functionShorthand = "(arr . uncurry $ (==))",
                    _functionLonghand = "(arr . uncurry $ (==))"
                }
            ])
        ],
        _shorthand = "eq",
        _longhand = "(arr . uncurry $ (==))"
    }

instance PrimitiveConsole JS where
    outputString = JS $ Code {
        _externalImports = [
            ("Control.Arrow", ["Kleisli(..)"])
        ],
        _internalImports = [
            ("Control.Category.Primitive.Bool", [
                Function {
                    _functionName = "outputString",
                    _functionTypeFrom = "String",
                    _functionTypeTo = "IO ()",
                    _functionShorthand = "Kleisli putStr",
                    _functionLonghand = "Kleisli putStr"
                }
            ])
        ],
        _shorthand = "outputString",
        _longhand = "Kleisli putStr"
    }
    inputString = JS $ Code {
        _externalImports = [
            ("Control.Arrow", ["Kleisli(..)"])
        ],
        _internalImports = [
            ("Control.Category.Primitive.Console", [
                Function {
                    _functionName = "inputString",
                    _functionTypeFrom = "()",
                    _functionTypeTo = "IO String",
                    _functionShorthand = "Kleisli (const getContents)",
                    _functionLonghand = "Kleisli (const getContents)"
                }
            ])
        ],
        _shorthand = "inputString",
        _longhand = "Kleisli (const getContents)"
    }

instance PrimitiveExtra JS where
    intToString = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Primitive.Extra", [
                Function {
                    _functionName = "intToString",
                    _functionTypeFrom = "Int",
                    _functionTypeTo = "String",
                    _functionShorthand = "show",
                    _functionLonghand = "show"
                }
                ]
            )
            ],
        _shorthand = "intToString",
        _longhand = "show"
    }
    concatString = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Primitive.Extra", [
                Function {
                    _functionName = "concatString",
                    _functionTypeFrom = "(String, String)",
                    _functionTypeTo = "String",
                    _functionShorthand = "uncurry (<>)",
                    _functionLonghand = "uncurry (<>)"
                }
                ]
            )
            ],
        _shorthand = "concatString",
        _longhand = "uncurry (<>)"
    }
    constString s = JS $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "const \"" <> BSL.pack s <> "\"",
        _longhand = "const \"" <> BSL.pack s <> "\""
    }

instance PrimitiveFile JS where
    readFile' = JS $ Code {
        _externalImports = [
            ("Control.Arrow", ["Kleisli(..)"]),
            ("Control.Category", ["(.)"]),
            ("Control.Monad.IO.Class", ["liftIO"])
        ],
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

instance PrimitiveString JS where
    reverseString = JS $ Code {
        _externalImports = [
            ("Control.Arrow", ["arr"])
        ],
        _internalImports = [
            ("Control.Category.Primitive.String", [
                Function {
                    _functionName = "reverseString",
                    _functionTypeFrom = "String",
                    _functionTypeTo = "String",
                    _functionShorthand = "arr reverse",
                    _functionLonghand = "arr reverse"
                }
                ]
            )
            ],
        _shorthand = "reverseString",
        _longhand = "arr reverse"
    }

instance Numeric JS where
    num n = JS $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "const " <> BSL.pack (show n),
        _longhand = "const " <> BSL.pack (show n)
    }
    negate' = JS $ Code {
        _externalImports = [],
        _internalImports = [],
        _shorthand = "negate",
        _longhand = "negate"
    }
    add = JS $ Code {
        _externalImports = [],
        _internalImports = [
            ("Control.Category.Numeric",  [
                Function {
                    _functionName = "add",
                    _functionTypeFrom = "(Int, Int)",
                    _functionTypeTo = "Int",
                    _functionShorthand = "uncurry (+)",
                    _functionLonghand = "uncurry (+)"
                }
                ]
            )
            ],
        _shorthand = "add",
        _longhand = "uncurry (+)"
    }
    mult = JS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "Control.Category.Numeric",
                [
                    Function {
                        _functionName = "mult",
                        _functionTypeFrom = "(Int, Int)",
                        _functionTypeTo = "Int",
                        _functionShorthand = "uncurry (*)",
                        _functionLonghand = "uncurry (*)"
                    }
                    ]
            )
            ],
        _shorthand = "mult",
        _longhand = "uncurry (*)"
    }
    div' = JS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "Control.Category.Numeric",
                [
                    Function {
                        _functionName = "div'",
                        _functionTypeFrom = "(Int, Int)",
                        _functionTypeTo = "Int",
                        _functionShorthand = "uncurry div",
                        _functionLonghand = "uncurry div"
                    }
                    ]
            )
            ],
        _shorthand = "div'",
        _longhand = "uncurry div"
    }
    mod' = JS $ Code {
        _externalImports = [],
        _internalImports = [
            (
                "Control.Category.Numeric",
                [
                    Function {
                        _functionName = "mod'",
                        _functionTypeFrom = "(Int, Int)",
                        _functionTypeTo = "Int",
                        _functionShorthand = "uncurry mod",
                        _functionLonghand = "uncurry mod"
                    }
                    ]
            )
            ],
        _shorthand = "mod'",
        _longhand = "uncurry mod"
    }

{-}
-- I don't quite know how to call ghci or cabal repl to include the correct functions here, so the tests are skipped.

-- @TODO escape shell - Text.ShellEscape?
-}

instance ExecuteJSLonghand JS where
    executeGHCiLonghand cat param = do
        let params ∷ [String]
            params = [
                -- "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementLonghand cat) <> ") (" <> show param <> ")"
                ]
        -- liftIO . putStrLn $ "Params"
        -- liftIO . print $ params
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        -- liftIO . putStrLn $ "Stdout"
        -- liftIO . putStrLn $ stdout
        -- liftIO . putStrLn $ "Stderr"
        -- liftIO . putStrLn $ stderr
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

instance ExecuteJSShorthand JS where
    executeGHCiShorthand cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                toShorthandCLIDefinitions cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementShorthand cat) <> ") (" <> show param <> ")"
                ]
        -- liftIO . putStrLn $ "Params"
        -- liftIO . print $ params
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        -- liftIO . putStrLn $ "Stdout"
        -- liftIO . putStrLn $ stdout
        -- liftIO . putStrLn $ "Stderr"
        -- liftIO . putStrLn $ stderr
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

instance ExecuteJSImports JS where
    executeGHCiImports cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                toInternalCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementShorthand cat) <> ") (" <> show param <> ")"
                ]
        -- liftIO . putStrLn $ "Params"
        -- liftIO . print $ params
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        -- liftIO . putStrLn $ "Stdout"
        -- liftIO . putStrLn $ stdout
        -- liftIO . putStrLn $ "Stderr"
        -- liftIO . putStrLn $ stderr
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

-- @TODO this passes too many arguments apparently...
-- This is because of the id and (.) using the (->) instance whereas I am running Kleisli below.
-- This means we need to deal with both within JS sessions. Let's try to use Pure/Monadic... or maybe JSPure / JSMonadic accepting only appropriate typeclasses / primitives?
instance ExecuteStdioLonghand JS where
    executeStdioLonghand cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", BSL.unpack (renderStatementLonghand cat) <> " ()"
                ]
        -- liftIO . putStrLn $ "Params"
        -- liftIO . print $ params
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        -- liftIO . putStrLn $ "Stdout"
        -- liftIO . putStrLn $ stdout
        -- liftIO . putStrLn $ "Stderr"
        -- liftIO . putStrLn $ stderr
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

instance ExecuteStdioImports JS where
    executeStdioImports cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)",
                -- ] <>
                -- toCLIImports cat <>
                -- [
                "-e", BSL.unpack (renderStatementShorthand cat) <> " ()"
                ]
        -- liftIO . putStrLn $ "Params"
        -- liftIO . print $ params
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        -- liftIO . putStrLn $ "Stdout"
        -- liftIO . putStrLn $ stdout
        -- liftIO . putStrLn $ "Stderr"
        -- liftIO . putStrLn $ stderr
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

instance ExecuteStdioShorthand JS where
    executeStdioShorthand cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":cd library",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", BSL.unpack (renderStatementShorthand cat) <> " ()"
                ]
        -- liftIO . putStrLn $ "Params"
        -- liftIO . print $ params
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        -- liftIO . putStrLn $ "Stdout"
        -- liftIO . putStrLn $ stdout
        -- liftIO . putStrLn $ "Stderr"
        -- liftIO . putStrLn $ stderr
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (readEither stdout)

instance ExecuteJSONLonghand JS where
    executeJSONLonghand cat param = do
        let params ∷ [String]
            params = [
                -- "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementLonghand cat) <> ") (" <> BSL.unpack (encode param) <> ")"
                ]
        -- liftIO . putStrLn $ "Params"
        -- liftIO . print $ params
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        -- liftIO . putStrLn $ "Stdout"
        -- liftIO . putStrLn $ stdout
        -- liftIO . putStrLn $ "Stderr"
        -- liftIO . putStrLn $ stderr
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (eitherDecode (BSL.pack stdout))

instance ExecuteJSONShorthand JS where
    executeJSONShorthand cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                toShorthandCLIDefinitions cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementShorthand cat) <> ") (" <> BSL.unpack (encode param) <> ")"
                ]
        -- liftIO . putStrLn $ "Params"
        -- liftIO . print $ params
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        -- liftIO . putStrLn $ "Stdout"
        -- liftIO . putStrLn $ stdout
        -- liftIO . putStrLn $ "Stderr"
        -- liftIO . putStrLn $ stderr
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (eitherDecode (BSL.pack stdout))

instance ExecuteJSONImports JS where
    executeJSONImports cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -ilibrary",
                "-e", ":set -XGHC2024",
                "-e", "import Prelude hiding ((.), id)"
                ] <>
                toExternalCLIImports cat <>
                toInternalCLIImports cat <>
                [
                "-e", "(" <> BSL.unpack (renderStatementShorthand cat) <> ") (" <> BSL.unpack (encode param) <> ")"
                ]
        -- liftIO . putStrLn $ "Params"
        -- liftIO . print $ params
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        -- liftIO . putStrLn $ "Stdout"
        -- liftIO . putStrLn $ stdout
        -- liftIO . putStrLn $ "Stderr"
        -- liftIO . putStrLn $ stderr
        case exitCode of
            ExitFailure code' -> liftIO . throwIO . userError $ "Exit code " <> show code' <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> either (liftIO . throwIO . userError . ("Can't parse response: " <>)) pure (eitherDecode (BSL.pack stdout))
-}