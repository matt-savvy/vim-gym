module Main (main) where
import System.Environment (getArgs)
import System.IO (withFile, IOMode(..), hGetContents)
import System.Process

data Command = AddDrill { path :: FilePath } | Review deriving (Show)

handleArgs :: [String] -> Command
handleArgs ["add", filePath] = AddDrill { path = filePath }
handleArgs ["review"] = Review
handleArgs _args = undefined

handleCommand :: Command -> IO ()
handleCommand (AddDrill filePath) = copyFile filePath
handleCommand Review = review

copyFile :: FilePath -> IO ()
copyFile filePath = do
    withFile filePath ReadMode processFile
    where newFilePath = "tmp_" <> filePath
          processFile handle = do
              contents <- hGetContents handle
              writeFile newFilePath contents

getDrill :: IO (Maybe FilePath)
getDrill = return $ Just "tmp_lorem.txt"

review :: IO ()
review = do
    drill <- getDrill
    case drill of
        Just filePath -> do
            _ <- runCommand $ vimCommand filePath
            print ""
        Nothing -> putStrLn "all done"

vimCommand :: FilePath -> String
vimCommand filePath = "/home/mattsavoia/.nix-profile/bin/vim" <> " " <> filePath

main :: IO ()
main = do
    args <- getArgs
    let command = handleArgs args
    handleCommand command
    print ""
