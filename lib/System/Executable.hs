{-# LANGUAGE Safe #-}

module System.Executable () where

-- import Data.ByteString.Lazy.Char8 qualified as BSL
-- import System.Console.GetOpt
-- import System.Environment

{-}
data CLIOptionsWithOutput = CLIOptionsWithOutput {
    input  :: Maybe String,
    output :: Maybe String
} deriving (Eq, Show)

defaultOptionsWithOutput ∷ CLIOptionsWithOutput
defaultOptionsWithOutput = CLIOptionsWithOutput {
    input = Nothing,
    output = Nothing
}

newtype CLIOptionsWithoutOutput = CLIOptionsWithoutOutput {
    singleInput :: Maybe String
}

defaultOptionsWithoutOutput ∷ CLIOptionsWithoutOutput
defaultOptionsWithoutOutput = CLIOptionsWithoutOutput {
    singleInput = Nothing
}

-- 'TODO maybe we need an Alternative instance?
insertOptionsWithOutputFromList ∷ [String] → CLIOptionsWithOutput → CLIOptionsWithOutput
insertOptionsWithOutputFromList [a] cliOptions@(CLIOptionsWithOutput Nothing _) = cliOptions { input = Just a }
insertOptionsWithOutputFromList [a] cliOptions@(CLIOptionsWithOutput (Just _) Nothing) = cliOptions { output = Just a }
insertOptionsWithOutputFromList [a, b] cliOptions@(CLIOptionsWithOutput Nothing Nothing) = cliOptions { input = Just a, output = Just b}
insertOptionsWithOutputFromList _ cliOptions = cliOptions

-- 'TODO maybe we need an Alternative instance?
insertOptionsWithoutOutputFromList ∷ [String] → CLIOptionsWithoutOutput → CLIOptionsWithoutOutput
insertOptionsWithoutOutputFromList [a] cliOptions@(CLIOptionsWithoutOutput Nothing) = cliOptions { singleInput = Just a }
insertOptionsWithoutOutputFromList _ cliOptions = cliOptions

optionsProducingOutput ∷ [OptDescr (CLIOptionsWithOutput → CLIOptionsWithOutput)]
optionsProducingOutput = [
        Option ['i'] ["input"] (ReqArg (\f cliOptions -> cliOptions { input = Just f } ) "FILE") "file to process",
        Option ['o'] ["output"] (ReqArg (\f cliOptions -> cliOptions { output = Just f } ) "FILE") "file to produce"
    ]

optionsNotProducingOutput ∷ [OptDescr (CLIOptionsWithoutOutput → CLIOptionsWithoutOutput)]
optionsNotProducingOutput = [
        Option ['i'] ["input"] (ReqArg (\f cliOptions -> cliOptions { singleInput = Just f } ) "FILE") "file to process"
    ]

parseAllProducingOutput ∷ IO CLIOptionsWithOutput
parseAllProducingOutput = do
    argv <- getArgs
    case getOpt Permute optionsProducingOutput argv of
        (o, n, []) -> do
            let parsed = foldl' (flip id) defaultOptionsWithOutput o
                includingUnparsed = insertOptionsWithOutputFromList n parsed
            pure includingUnparsed
        (_, _, errs) -> do
            progName <- getProgName
            ioError (userError (concat errs <> usageInfo (header progName) optionsProducingOutput))
    where header progName = "Usage: " <> progName <> " [OPTION...] files..."

parseAllNotProducingOutput ∷ IO CLIOptionsWithoutOutput
parseAllNotProducingOutput = do
    argv <- getArgs
    case getOpt Permute optionsNotProducingOutput argv of
        (o, n, []) -> do
            let parsed = foldl' (flip id) defaultOptionsWithoutOutput o
                includingUnparsed = insertOptionsWithoutOutputFromList n parsed
            pure includingUnparsed
        (_, _, errs) -> do
            progName <- getProgName
            ioError (userError (concat errs <> usageInfo (header progName) optionsNotProducingOutput))
    where header progName = "Usage: " <> progName <> " [OPTION...] files..."

getFileOrContents ∷ Maybe String → IO BSL.ByteString
getFileOrContents = maybe BSL.getContents BSL.readFile

writeFileOrStdout ∷ Maybe String → BSL.ByteString → IO ()
writeFileOrStdout = maybe BSL.putStr BSL.writeFile

readToWrite ∷ (BSL.ByteString → IO BSL.ByteString) → IO ()
readToWrite transformer = do
    parsed <- parseAllProducingOutput

    getFileOrContents (input parsed) >>= transformer >>= writeFileOrStdout (output parsed)

readToOp ∷ (BSL.ByteString → IO a) → IO a
readToOp transformer = do
    parsed <- parseAllNotProducingOutput

    getFileOrContents (singleInput parsed) >>= transformer
-}
{-}
codec :: Category cat => (BSL.ByteString → IO (FreeFunc Prims () ())) => (FreeFunc Prims () () → IO BSL.ByteString) -> IO ()
codec decoder encoder = readToWrite ((pure . encoder) <=< decoder)


run :: (BSL.ByteString → IO (FreeFunc Prims () ())) -> (FreeFunc Prims () () → IO (Kleisli IO () ())) -> IO ()
run decoder interpreter = readToOp ((flip runKleisli () . pure interpreter) <=< decoder)

transpile ∷ Category cat =>
    (cat () () -> BSL.ByteString) -> (BSL.ByteString → IO (FreeFunc Prims () ())) -> (FreeFunc Prims () () → IO (cat () ())) -> IO ()
transpile renderer decoder interpreter = readToWrite (
    \bs -> renderer . (interpreter <$> decoder bs)
    )
-}