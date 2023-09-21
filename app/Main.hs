module Main (main) where
import System.Environment (getArgs)
import System.IO (withFile, IOMode(..), hGetContents)

data Command = AddDrill { path :: FilePath } deriving (Show)

handleArgs :: [String] -> Command
handleArgs ["add", filePath] = AddDrill { path = filePath }
handleArgs _args = undefined

handleCommand :: Command -> IO ()
handleCommand (AddDrill filePath) = copyFile filePath

copyFile :: FilePath -> IO ()
copyFile filePath = do
    withFile filePath ReadMode processFile
    where newFilePath = "tmp_" <> filePath
          processFile handle = do
              contents <- hGetContents handle
              writeFile newFilePath contents

main :: IO ()
main = do
    args <- getArgs
    let command = handleArgs args
    handleCommand command
