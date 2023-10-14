{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

-- | Mock module, same as Func but doesn't do any real I/O.
module Data.Code.Haskell.Mock where

import Control.Category
-- import Control.Category.Apply
import Control.Category.Bracket
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Execute.Haskell
import Control.Category.Execute.Stdio
import Control.Category.Numeric
import Control.Category.Primitive.Abstract
import Control.Category.Primitive.Console
import Control.Category.Primitive.Extra
import Control.Category.Primitive.File
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

newtype HSMock a b = HSMock BSL.ByteString
    deriving (Eq, Show)

instance IsString (HSMock a b) where
    fromString = HSMock . BSL.pack

instance RenderStatement (HSMock a b) where
    renderStatement (HSMock f) = f

instance Bracket HSMock where
    bracket s = HSMock $ "(" <> renderStatement s <> ")"

instance Category HSMock where
    id = "id"
    a . b = bracket $ HSMock (renderStatement a <> " . " <> renderStatement b)

instance Cartesian HSMock where
    copy = bracket "\\x -> (x, x)"
    consume = bracket "const ()"
    fst' = "fst"
    snd' = "snd"

instance Cocartesian HSMock where
    injectL = "Left"
    injectR = "Right"
    unify = bracket "\\case { Left a -> a; Right a -> a; }"
    tag = bracket "\\case { (False, a) -> Left a; (True, a) -> Right a; }"

instance Strong HSMock where
    first' f = HSMock $ "(Data.Bifunctor.first " <> renderStatement f <> ")"
    second' f = HSMock $ "(Data.Bifunctor.second " <> renderStatement f <> ")"

instance Choice HSMock where
    left' f = HSMock $ "(\\case { Left a -> Left (" <> renderStatement f <> " a); Right a -> Right a; })"
    right' f = HSMock $ "(\\case { Left a -> Left a; Right a -> Right (" <> renderStatement f <> " a); })"

instance Symmetric HSMock where
    swap = "(\\(a, b) -> (b, a))"
    swapEither = "(\\case { Left a -> Right a; Right a -> Left a; })"
    reassoc = "(\\(a, (b, c)) -> ((a, b), c))"
    reassocEither = "(\\case { Left a -> Left (Left a); Right (Left b) -> Left (Right b); Right (Right c) -> Right c })"

-- instance Cochoice HSMock where

-- instance Costrong HSMock where

-- instance Apply HSMock where

instance Primitive HSMock where
    eq = "(arr . uncurry $ (==))"
    reverseString = "(arr reverse)"

instance PrimitiveConsole HSMock where
    outputString = "(Kleisli putStr)"
    inputString = "(Kleisli (const getContents))"

instance PrimitiveExtra HSMock where
    intToString = "show"
    concatString = "(uncurry (<>))"
    constString s = HSMock $ "(const \"" <> BSL.pack s <> "\")"

-- @TODO some kind of debug (stderr?)
instance PrimitiveFile HSMock where
    readFile' = ""
    writeFile' = ""

instance Numeric HSMock where
    num n = HSMock $ "(const " <> fromString (show n) <> ")"
    negate' = "negate"
    add = "(uncurry (+))"
    mult = "(uncurry (*))"
    div' = "(uncurry div)"
    mod' = "(uncurry mod)"

-- @TODO escape shell - Text.ShellEscape?
instance ExecuteHaskell HSMock where
    executeViaGHCi cat param = do
        let params ∷ [String]
            params = [
                "-e", ":set -XLambdaCase",
                "-e", "import Control.Arrow",
                "-e", "import Prelude hiding ((.), id)",
                "-e", "import Control.Category",
                "-e", "(" <> BSL.unpack (renderStatement cat) <> ") (" <> show param <> ")"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params "")
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case readEither stdout of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

-- @TODO this uses runKleisli, the above does not

instance ExecuteStdio HSMock where
    executeViaStdio cat stdin = do
        let params ∷ [String]
            params = [
                "-e", ":set -XLambdaCase",
                "-e", "import Control.Arrow",
                "-e", "import Prelude hiding ((.), id)",
                "-e", "import Control.Category",
                "-e", "runKleisli (" <> BSL.unpack (renderStatement cat) <> ") ()"
                ]
        (exitCode, stdout, stderr) <- liftIO (readProcessWithExitCode "ghci" params (show stdin))
        case exitCode of
            ExitFailure code -> liftIO . throwIO . userError $ "Exit code " <> show code <> " when attempting to run ghci with params: " <> unwords params <> " Output: " <> stderr
            ExitSuccess -> case readEither stdout of
                Left err -> liftIO . throwIO . userError $ "Can't parse response: " <> err
                Right ret -> pure ret

-- @ TODO JSON
