{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE Unsafe                     #-}
{-# OPTIONS_GHC -Wno-safe -Wno-unsafe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Code.Generic where

import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map                   (Map)
import Data.Map                   qualified as M
import Data.Maybe
import Data.Set                   (Set)
import Data.Set                   qualified as S
import Data.String
import GHC.IsList

type Module = BSL.ByteString
type FunctionName = BSL.ByteString
type FunctionType = BSL.ByteString
type Shorthand = BSL.ByteString
type Definition = BSL.ByteString

newtype Imports = Imports {
    unImports :: Map Module (Set (FunctionName, Maybe Definition))
}
    deriving stock (Eq, Show)
    deriving newtype IsList

filterExternal ∷ Imports → Imports
filterExternal = Imports . M.filter (not . null) . fmap (S.filter (isNothing . snd)) . unImports

instance Semigroup Imports where
    Imports a <> Imports b = Imports $ M.unionWith (<>) a b

deriving instance Monoid Imports

data Export = Export {
    module'      :: Module,
    functionName :: FunctionName,
    functionType :: FunctionType
} deriving (Eq, Show)

-- TODO makeClassy

class HasImports a where
    imports :: a → Imports

class HasExport a where
    export :: a → Maybe Export

class HasShorthand a where
    shorthand :: a → Shorthand

class HasDefinition a where
    definition :: a →  Definition

data Code a b = Code {
    _imports    :: Imports,
    _export     :: Maybe Export,
    _shorthand  :: Shorthand,
    _definition :: Definition
} deriving (Eq, Show)

instance IsString (Code a b) where
    fromString s = Code [] Nothing (BSL.pack s) (BSL.pack s)

class HasCode f a b where
    code :: f a b → Code a b
    -- wrap :: Code a b -> f a b

class MkCode f a b where
    mkCode :: Imports -> Maybe Export -> Shorthand -> Definition -> f a b

instance MkCode f a b => IsString (f a b) where
    fromString s = mkCode [] Nothing (BSL.pack s) (BSL.pack s)

instance MkCode Code a b where
    mkCode = Code

instance HasCode Code a b where
    code :: Code a b → Code a b
    code = id

instance HasCode f a b ⇒ HasImports (f a b) where
    imports = imports . code

instance HasCode f a b => HasExport (f a b) where
    export = export . code

instance HasCode f a b => HasShorthand (f a b) where
    shorthand = shorthand . code

instance HasCode f a b => HasDefinition (f a b) where
    definition = definition . code

toImports ∷ (HasExport c, HasDefinition c) => c → Imports
toImports c = maybe mempty (\Export { module' = module', functionName = functionName' } -> Imports [(module', [(functionName', Just (definition c))])]) (export c)


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