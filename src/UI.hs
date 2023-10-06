module UI (getContinue) where
import Data.Text (Text, toLower)
import qualified Data.Text.IO as TIO

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
    where cleanInput = toLower input
          parseContinue' "y" = Just True
          parseContinue' "n" = Just False
          parseContinue' _ = Nothing


