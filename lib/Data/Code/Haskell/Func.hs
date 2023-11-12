{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

-- | Main Haskell Func module. Runs categories as Haskell functions on the command line.
module Data.Code.Haskell.Func where

import Control.Category
-- import Control.Category.Apply
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.Haskell
import Control.Category.Execute.Stdio
import Control.Category.Numeric
import Control.Category.Primitive.Bool
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Control.Category.Primitive.File
import Control.Category.Primitive.String
import Control.Category.Strong
import Control.Category.Symmetric
import Control.Exception                   hiding (bracket)
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8          qualified as BSL
import Data.Render.Statement
import Data.String
import GHC.IO.Exception
import Prelude                             hiding (id, (.))
import System.Process
import Text.Read

newtype HSFunc a b = HSFunc BSL.ByteString
    deriving (Eq, Show)

instance IsString (HSFunc a b) where
    fromString = HSFunc . BSL.pack

instance RenderStatement (HSFunc a b) where
    renderStatement (HSFunc f) = f

instance Bracket HSFunc where
    bracket s = HSFunc $ "(" <> renderStatement s <> ")"

instance Category HSFunc where
    id = "id"
    a . b = bracket $ HSFunc (renderStatement a <> " . " <> renderStatement b)

instance Cartesian HSFunc where
    copy :: HSFunc a (a, a)
    copy = bracket "\\x -> (x, x)"
    consume = bracket "const ()"
    fst' = "fst"
    snd' = "snd"

instance Cocartesian HSFunc where
    injectL = "Left"
    injectR = "Right"
    unify = bracket "\\case { Left a -> a; Right a -> a; }"
    tag = bracket "\\case { (False, a) -> Left a; (True, a) -> Right a; }"

instance Strong HSFunc where
    first' f = HSFunc $ "(Data.Bifunctor.first " <> renderStatement f <> ")"
    second' f = HSFunc $ "(Data.Bifunctor.second " <> renderStatement f <> ")"

instance Choice HSFunc where
    left' f = HSFunc $ "(\\case { Left a -> Left (" <> renderStatement f <> " a); Right a -> Right a; })"
    right' f = HSFunc $ "(\\case { Left a -> Left a; Right a -> Right (" <> renderStatement f <> " a); })"

instance Symmetric HSFunc where
    swap = "(\\(a, b) -> (b, a))"
    swapEither = "(\\case { Left a -> Right a; Right a -> Left a; })"
    reassoc = "(\\(a, (b, c)) -> ((a, b), c))"
    reassocEither = "(\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c })"

-- instance Cochoice HSFunc where

-- instance Costrong HSFunc where

-- instance Apply HSFunc where

instance PrimitiveBool HSFunc where
    eq = "(arr . uncurry $ (==))"

instance PrimitiveConsole HSFunc where
    outputString = "(Kleisli putStr)"
    inputString = "(Kleisli (const getContents))"

instance PrimitiveExtra HSFunc where
    intToString = "show"
    concatString = "(uncurry (<>))"
    constString s = HSFunc $ "(const \"" <> BSL.pack s <> "\")"

instance PrimitiveFile HSFunc where
    readFile' = "(Kleisli $ liftIO . readFile)"
    writeFile' = "(Kleisli $ liftIO . uncurry writeFile)"

instance PrimitiveString HSFunc where
    reverseString = "(arr reverse)"

instance Numeric HSFunc where
    num n = HSFunc $ "(const " <> fromString (show n) <> ")"
    negate' = "negate"
    add = "(uncurry (+))"
    mult = "(uncurry (*))"
    div' = "(uncurry div)"
    mod' = "(uncurry mod)"

-- @TODO escape shell - Text.ShellEscape?
instance ExecuteHaskell HSFunc where
    executeViaGHCi cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -XLambdaCase",
                "-e", "import Control.Arrow",
                "-e", "import Prelude hiding ((.), id)",
                "-e", "import Control.Category",
                "-e", "import Control.Monad.IO.Class",
                "-e", "(" <> BSL.unpack (renderStatement cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case readEither stdout of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

-- @TODO this uses runKleisli, the above does not

instance ExecuteStdio HSFunc where
    executeViaStdio cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":set -XLambdaCase",
                "-e", "import Control.Arrow",
                "-e", "import Prelude hiding ((.), id)",
                "-e", "import Control.Category",
                "-e", "import Control.Monad.IO.Class",
                "-e", "runKleisli (" <> BSL.unpack (renderStatement cat) <> ") ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case readEither stdout of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

-- @ TODO JSON
