module UI (getContinue, getScore, Command (..), handleArgs, showStatus) where

import Data.Text (Text, pack, toLower)
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

getContinue :: IO Bool
getContinue = do
    putStrLn "continue? y/n"
    input <- TIO.getLine
    let continue = parseContinue input
    case continue of
        Nothing -> do
            putStr "invalid input. "
            getContinue
        Just continue' -> return continue'

parseContinue :: Text -> Maybe Bool
parseContinue input = parseContinue' cleanInput
    where
        cleanInput = toLower input
        parseContinue' "y" = Just True
        parseContinue' "n" = Just False
        parseContinue' _ = Nothing

getScore :: IO Int
getScore = do
    putStrLn "how did you do? score from 0 - 5, ? for help"
    line <- getLine
    case line of
        "?" -> do
            showHelpText
            getScore
        _line -> do
            let maybeScore = readMaybe line :: Maybe Int
            handleScore maybeScore
    where
        handleScore :: Maybe Int -> IO Int
        handleScore (Just score) | validScore score = return score
        handleScore _invalidScore = do
            putStrLn "Score not recognized."
            getScore
        validScore score = score >= 0 && score <= 5

showHelpText :: IO ()
showHelpText = TIO.putStrLn helpText
    where
        helpText =
            "SCORING\n \
            \- 0 Total blackout, complete failure to recall the information.\n \
            \- 1 Incorrect response, but upon seeing the correct answer it felt familiar.\n \
            \- 2 Incorrect response, but upon seeing the correct answer it seemed easy to remember.\n \
            \- 3 Correct response, but required significant effort to recall.\n \
            \- 4 Correct response, after some hesitation.\n \
            \- 5 Correct response with perfect recall."

showStatus :: Int -> IO ()
showStatus n = TIO.putStrLn $ "Drills due for review: " <> (pack . show) n

data Command = AddDrill {path :: FilePath} | Review | Status deriving (Show)

handleArgs :: [String] -> Command
handleArgs ["add", filePath] = AddDrill {path = filePath}
handleArgs ["review"] = Review
handleArgs ["status"] = Status
handleArgs _args = undefined
