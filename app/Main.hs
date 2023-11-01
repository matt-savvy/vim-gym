module Main (main) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Time (Day (..), getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Database.SQLite.Simple
import Drill (Drill (..), initDrill)
import qualified FilePathHelper
import qualified Queries
import qualified SM2
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Environment (getArgs, lookupEnv)
import System.IO (IOMode (..), withFile)
import System.Process
import qualified UI

data File = File {fileDrillId :: Int, fileName :: FilePath, fileBody :: Text.Text} deriving (Show)
instance ToRow File where
    toRow (File drillId' filename body) = toRow (drillId', filename, body)

handleCommand :: UI.Command -> Connection -> IO ()
handleCommand (UI.AddDrill []) _conn = undefined
handleCommand (UI.AddDrill filePaths) conn = addDrill filePaths conn
handleCommand UI.Review conn = review conn
handleCommand (UI.ReviewManual drillId') conn = reviewManual conn drillId'
handleCommand UI.Status conn = status conn
handleCommand UI.List conn = list conn

addDrill :: [FilePath] -> Connection -> IO ()
addDrill filePaths conn = do
    let drill = initDrill
    execute conn Queries.insertDrillQuery drill
    drillRowId <- lastInsertRowId conn
    let drillId' = fromIntegral drillRowId
    mapM_ (addFile drillId') filePaths
    putStrLn ("Added drill for " <> unwords filePaths)
    where
        addFile drillId' filePath =
            withFile
                filePath
                ReadMode
                ( \handle -> do
                    contents <- TIO.hGetContents handle
                    let file = File {fileDrillId = drillId', fileName = filePath, fileBody = contents}
                    execute conn Queries.insertFileQuery file
                )

getDrill :: Connection -> Int -> IO (Maybe Drill)
getDrill conn drillId' = do
    drillRows <- query conn Queries.getDrillQuery (Only drillId')
    let drills = map toDrill drillRows
    return $ case drills of
        [] -> Nothing
        (drill : _rest) -> Just drill

getDue :: Connection -> IO [Drill]
getDue conn = do
    drillRows <- query_ conn Queries.getDueQuery
    return $ map toDrill drillRows

getFiles :: Connection -> Int -> IO [File]
getFiles conn drillId' = do
    fileRows <- query conn Queries.getFilesQuery (Only drillId')
    return $ map toFile fileRows

toDrill :: (Int, Int, Float, Int, String) -> Drill
toDrill (id', streak, score, interval, lastReviewed) =
    Drill
        { drillId = id'
        , drillStreak = streak
        , drillScore = score
        , drillInterval = interval
        , drillLastReviewed = lastReviewTime
        }
    where
        lastReviewTime = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" lastReviewed

toDrillWithFilenames :: (Int, Int, Float, Int, String, String) -> (Drill, String)
toDrillWithFilenames (id', streak, score, interval, lastReviewed, filenames) =
    (drill, filenames)
    where
        drill = toDrill (id', streak, score, interval, lastReviewed)

toFile :: (Int, FilePath, Text.Text) -> File
toFile (drillId', name, body) = File {fileDrillId = drillId', fileName = name, fileBody = body}

createTmpDir :: IO ()
createTmpDir = createDirectoryIfMissing True tmpDir

review :: Connection -> IO ()
review conn = do
    drills <- getDue conn
    review' drills
    where
        review' [] = putStrLn "Nothing left to review."
        review' (drill : _rest) = do
            reviewDrill conn drill
            drills <- getDue conn
            continue drills
        continue [] = review' []
        continue drills = do
            willContinue <- UI.getContinue
            when willContinue $ review' drills

reviewDrill :: Connection -> Drill -> IO ()
reviewDrill conn drill = do
    files <- getFiles conn (drillId drill)
    reviewFiles files
    newScore <- UI.getScore
    currentDay <- getCurrentDay
    let newDrill' = applySM2Grade drill (SM2.applyScore (fromIntegral newScore) (toSM2Grade drill))
    let newDrill = newDrill' {drillLastReviewed = currentDay}
    execute conn Queries.updateDrillQuery (drillStreak newDrill, drillScore newDrill, drillInterval newDrill, drillLastReviewed newDrill, drillId newDrill)
    where
        reviewFiles [] = return ()
        reviewFiles files = do
            createTmpDir
            mapM_
                ( \file -> do
                    let fullPath = tmpDir <> fileName file
                    let (dirPath, _filename) = FilePathHelper.split fullPath
                    createDirectoryIfMissing True dirPath
                    TIO.writeFile fullPath (fileBody file)
                )
                files
            let filenames = map (\file -> tmpDir <> fileName file) files
            vim <- getVim
            callCommand $ vimCommand vim filenames
            removeDirectoryRecursive tmpDir

reviewManual :: Connection -> Int -> IO ()
reviewManual conn drillId' = do
    drill <- getDrill conn drillId'
    maybeReviewDrill drill
    where
        maybeReviewDrill Nothing = putStrLn "No drill found"
        maybeReviewDrill (Just drill') = reviewDrill conn drill'

tmpDir :: String
tmpDir = "/tmp/vim-gym/"

getVim :: IO String
getVim = do
    maybeVim <- lookupEnv "VIM_COMMAND"
    let vim = fromMaybe "vim" maybeVim
    return vim

toSM2Grade :: Drill -> SM2.Grade
toSM2Grade drill =
    SM2.Grade (drillStreak drill) (drillScore drill) (drillInterval drill)

applySM2Grade :: Drill -> SM2.Grade -> Drill
applySM2Grade drill (SM2.Grade streak score interval) =
    drill {drillStreak = streak, drillScore = score, drillInterval = interval}

getCurrentDay :: IO Day
getCurrentDay = do
    utctDay <$> getCurrentTime

vimCommand :: String -> [FilePath] -> String
vimCommand vim filePaths =
    unwords $ ["(", "cd", tmpDir, ";", vim] <> filePaths <> [")"]

getDrillsDueCount :: Connection -> IO Int
getDrillsDueCount conn = do
    rows <- query_ conn Queries.getDueCountQuery :: IO [Only Int]
    case rows of
        [Only {fromOnly = count}] -> return count
        _ -> return 0

status :: Connection -> IO ()
status conn = do
    drillsDueCount <- getDrillsDueCount conn
    UI.showStatus drillsDueCount

listDrills :: Connection -> IO [(Drill, String)]
listDrills conn = do
    rows <- query_ conn Queries.listDrillsQuery
    return $ map toDrillWithFilenames rows

list :: Connection -> IO ()
list conn = do
    drills <- listDrills conn
    UI.showDrills drills

main :: IO ()
main = do
    args <- getArgs
    let command = UI.handleArgs args
    withConnection "vim-gym.db" $ \conn -> do
        handleCommand command conn
