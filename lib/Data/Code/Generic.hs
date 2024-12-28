{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE Unsafe                     #-}
{-# OPTIONS_GHC -Wno-safe -Wno-unsafe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Code.Generic where

import Data.ByteString.Lazy.Char8 qualified as BSL
-- import Data.Map                   (Map)
-- import Data.Map                   qualified as M
import Data.MapSet
-- import Data.Maybe
-- import Data.Set                   (Set)
-- import Data.Set                   qualified as S
-- import Data.String
-- import GHC.IsList

type Module = BSL.ByteString

{-
e.g. HS

print a concatenated version of two strings

module = Main
externalImports = [
    (
        module = Prelude
        functionName = putStrLn
    )
    (
        module = Prelude
        functionName = (.)
    )
    ]
internalImports = [
    (
        module = Primitive.Concat
        functionName = concatStrings
        functionTypeFrom = (String, String)
        functionTypeTo = String
        shorthand = "uncurry (<>)"
        longhand = "(\(x, y) -> x <> y)"
    )
]
shorthand = putStrLn . concatStrings
longhand = putStrLn . (\(x, y) -> x <> y)
-}

type FunctionName = BSL.ByteString

type FunctionTypeFrom = BSL.ByteString

type FunctionTypeTo = BSL.ByteString

-- | Includes the function name to define the full (composed) function.
-- e.g. a(b(c))
type Shorthand = BSL.ByteString

-- For functions
type ShorthandDefinition = BSL.ByteString

-- | Includes the longhand to define the full (composed) function.
-- e.g. (\x y z -> x (y z))(+1)(+ 1)(2)
type Longhand = BSL.ByteString

-- For functions
type LonghandDefinition = BSL.ByteString

class HasExternalImports a where
    externalImports :: a → ExternalImports

class HasInternalImports a where
    internalImports :: a → InternalImports

class HasModule a where
    module' :: a → Module

class HasFunction a where
    function :: a → Function

class HasFunctionName a where
    functionName :: a → FunctionName

class HasFunctionTypeFrom a where
    functionTypeFrom :: a → FunctionTypeFrom

class HasFunctionTypeTo a where
    functionTypeTo :: a → FunctionTypeTo

class HasShorthand a where
    shorthand :: a → Shorthand

class HasLonghand a where
    longhand :: a → Longhand

class HasShorthandDefinition a where
    functionShorthand :: a → ShorthandDefinition

class HasLonghandDefinition a where
    functionLonghand :: a → LonghandDefinition

-- | A single function to be imported.
data Function = Function {
    _functionName     :: FunctionName,
    _functionTypeFrom :: FunctionTypeFrom,
    _functionTypeTo   :: FunctionTypeTo,
    _functionShorthand        :: ShorthandDefinition,
    _functionLonghand         :: LonghandDefinition
} deriving (Eq, Show, Ord)

instance HasFunctionName Function where
    functionName = _functionName

instance HasFunctionTypeFrom Function where
    functionTypeFrom = _functionTypeFrom

instance HasFunctionTypeTo Function where
    functionTypeTo = _functionTypeTo

instance HasShorthandDefinition Function where
    functionShorthand = _functionShorthand

instance HasLonghandDefinition Function where
    functionLonghand = _functionLonghand

instance HasFunction Function where
    function = id

type ExternalImports = MapSet Module FunctionName

type InternalImports = MapSet Module Function

-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> [("a", ["b"])] <> [("a", ["c"])] <> [("b", ["a", "b"]), ("c", ["a", "b"])] <> [("c", ["a", "c"]), ("a", ["a"])] :: Map Module (Set FunctionName)
-- fromList [("a",fromList ["b"]),("b",fromList ["a","b"]),("c",fromList ["a","b"])]

-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> [("a", ["b"])] <> [("a", ["c"])] <> [("b", ["a", "b"]), ("c", ["a", "b"])] <> [("c", ["a", "c"]), ("a", ["a"])] :: MapSet Module FunctionName
-- MapSet {getMapSet = fromList [("a",fromList ["a","b","c"]),("b",fromList ["a","b"]),("c",fromList ["a","b","c"])]}


-- TODO makeClassy
-- TODO capabilities?
-- class Has i a where


-- | Internal implementation of any programming language's code.
data Code a b = Code {
    -- | Anything outside of the project this function requires.
    _externalImports :: ExternalImports,
    -- | Anything inside of the project this function requires, including whether and how to export itself.
    _internalImports :: InternalImports,
    -- | The name of the current module. Probably "Main" unless making a library.
    -- @TODO Figure out whether we should be using Module here or in function
    -- it depends because we need to know whether to map Module to Set of Functions, or just have Set of Functions,
    -- when defining internal imports.
    -- TODO
    _shorthand :: Shorthand,
    _longhand :: Longhand
} deriving (Eq, Show)

-- instance IsString (Code a b) where
--     fromString s = Code [] Nothing (BSL.pack s) (BSL.pack s)

class HasCode f a b where
    code :: f a b → Code a b
    -- wrap :: Code a b -> f a b

instance HasCode Code a b where
    code :: Code a b → Code a b
    code = id

instance HasCode f a b ⇒ HasExternalImports (f a b) where
    externalImports = _externalImports . code

instance HasCode f a b ⇒ HasInternalImports (f a b) where
    internalImports = _internalImports . code

-- instance HasCode f a b ⇒ HasExport (f a b) where
--     export = export . code

instance HasCode f a b => HasShorthand (f a b) where
    shorthand = shorthand . code

instance HasCode f a b => HasLonghand (f a b) where
    longhand = longhand . code

instance HasFunction (f a b) ⇒ HasFunctionName (f a b) where
    functionName = functionName . function

instance HasFunction (f a b) ⇒ HasFunctionTypeFrom (f a b) where
    functionTypeFrom = functionTypeFrom . function

instance HasFunction (f a b) ⇒ HasFunctionTypeTo (f a b) where
    functionTypeTo = functionTypeTo . function

instance HasFunction (f a b) ⇒ HasShorthandDefinition (f a b) where
    functionShorthand = functionShorthand . function

instance HasFunction (f a b) ⇒ HasLonghandDefinition (f a b) where
    functionLonghand = functionLonghand . function

{-
toImports ∷ (HasDefinition c) ⇒ c → Imports
toImports c = Imports [
        (
            module',
            [
                (
                    functionName',
                    Just (longhand c)
                )
                ]
        )
        ]
    ) (export c)
-}


{-}
class IsoNT unwrapped wrapped where
    wrap :: unwrapped -> wrapped
    unwrap :: wrapped -> unwrapped

instance HasCode f a b => IsoNT (f a b) (Code f a b) where
    unwrap = code
-}

{-}
instance (HasCode f a b, IsoNT (f a b) ) => IsString (f a b) where
    fromString =
-}
