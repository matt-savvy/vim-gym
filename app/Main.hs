module Main (main) where

import Control.Monad (when)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Time (Day (..), getCurrentTime, utctDay)
import Database.SQLite.Simple
import qualified Queries
import qualified SM2
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.Environment (getArgs)
import System.IO (IOMode (..), withFile)
import System.Process
import qualified UI

data File = File {fileDrillId :: Int, fileName :: FilePath, fileBody :: Text.Text} deriving (Show)
instance ToRow File where
    toRow (File drillId' filename body) = toRow (drillId', filename, body)

data Drill = Drill
    { drillId :: Int
    , drillStreak :: Int
    , drillScore :: Float
    , drillInterval :: Int
    , drillLastReviewed :: Day
    }
    deriving (Show)

instance ToRow Drill where
    toRow (Drill _id streak score interval lastReviewed) = toRow (streak, score, interval, lastReviewed)

initDrill :: Drill
initDrill =
    Drill
        { drillId = 0
        , drillStreak = 0
        , drillScore = 2.5
        , drillInterval = 1
        , drillLastReviewed = ModifiedJulianDay 0
        }

handleCommand :: UI.Command -> Connection -> IO ()
handleCommand (UI.AddDrill []) _conn = undefined
handleCommand (UI.AddDrill filePaths) conn = addDrill filePaths conn
handleCommand UI.Review conn = review conn
handleCommand UI.Status conn = status conn

addDrill :: [FilePath] -> Connection -> IO ()
addDrill filePaths conn = do
    let grade = initDrill
    execute conn Queries.insertDrillQuery grade
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

getGrades :: Connection -> IO [Drill]
getGrades conn = do
    grades <- query_ conn Queries.getDueQuery
    return $ map toGrade grades

getFiles :: Connection -> Int -> IO [File]
getFiles conn gradeId' = do
    fileRows <- query conn Queries.getFilesQuery (Only gradeId')
    return $ map toFile fileRows

toGrade :: (Int, Int, Float, Int) -> Drill
toGrade (gradeId', streak, score, interval) =
    Drill
        { drillId = gradeId'
        , drillStreak = streak
        , drillScore = score
        , drillInterval = interval
        , drillLastReviewed = ModifiedJulianDay 0
        }

toFile :: (Int, FilePath, Text.Text) -> File
toFile (drillId', name, body) = File {fileDrillId = drillId', fileName = name, fileBody = body}

review :: Connection -> IO ()
review conn = do
    grades <- getGrades conn
    review' grades
    where
        reviewFiles [] = return ()
        reviewFiles files = do
            createDirectory tmpDir
            mapM_
                ( \file -> do
                    let filename = tmpDir <> fileName file
                    TIO.writeFile filename (fileBody file)
                )
                files
            let filenames = map (\file -> tmpDir <> fileName file) files
            callCommand $ vimCommand filenames
            removeDirectoryRecursive tmpDir
        review' [] = putStrLn "Nothing left to review."
        review' (grade' : _rest) = do
            files <- getFiles conn (drillId grade')
            reviewFiles files
            newScore <- UI.getScore
            currentDay <- getCurrentDay
            let newGrade' = applySM2Grade grade' (SM2.applyScore (fromIntegral newScore) (toSM2Grade grade'))
            let newGrade = newGrade' {drillLastReviewed = currentDay}
            execute conn Queries.updateDrillQuery (drillStreak newGrade, drillScore newGrade, drillInterval newGrade, drillLastReviewed newGrade, drillId newGrade)
            grades' <- getGrades conn
            continue grades'
        continue [] = review' []
        continue grades' = do
            willContinue <- UI.getContinue
            when willContinue $ review' grades'

tmpDir :: String
tmpDir = "/tmp/vim-gym/"

toSM2Grade :: Drill -> SM2.Grade
toSM2Grade grade =
    SM2.Grade (drillStreak grade) (drillScore grade) (drillInterval grade)

applySM2Grade :: Drill -> SM2.Grade -> Drill
applySM2Grade grade (SM2.Grade streak score interval) =
    grade {drillStreak = streak, drillScore = score, drillInterval = interval}

getCurrentDay :: IO Day
getCurrentDay = do
    utctDay <$> getCurrentTime

vimCommand :: [FilePath] -> String
vimCommand filePaths = "vim " <> unwords filePaths

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

main :: IO ()
main = do
    args <- getArgs
    let command = UI.handleArgs args
    withConnection "vim-gym.db" $ \conn -> do
        handleCommand command conn
