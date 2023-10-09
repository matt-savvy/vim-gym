module Main (main) where

import Control.Monad (when)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Time (Day (..), getCurrentTime, utctDay)
import Database.SQLite.Simple
import qualified Queries
import qualified SM2
import System.Environment (getArgs)
import System.IO (IOMode (..), withFile)
import System.Process
import qualified UI

data Drill = Drill {drillId :: Int, drillFileName :: FilePath, drillBody :: Text.Text} deriving (Show)
data File = File {fileDrillId :: Int, fileName :: FilePath, fileBody :: Text.Text} deriving (Show)

instance ToRow Drill where
    toRow (Drill _id fileName body) = toRow (fileName, body)

instance ToRow File where
    toRow (File drillId' filename body) = toRow (drillId', filename, body)

data Grade = Grade
    { gradeId :: Int
    , gradeDrill :: Drill
    , gradeStreak :: Int
    , gradeScore :: Float
    , gradeInterval :: Int
    , gradeLastReviewed :: Day
    }
    deriving (Show)

instance ToRow Grade where
    toRow (Grade _id drill streak score interval lastReviewed) = toRow (drillId drill, streak, score, interval, lastReviewed)

initGrade :: Drill -> Grade
initGrade drill =
    Grade
        { gradeId = 0
        , gradeDrill = drill
        , gradeStreak = 0
        , gradeScore = 2.5
        , gradeInterval = 1
        , gradeLastReviewed = ModifiedJulianDay 0
        }

handleCommand :: UI.Command -> Connection -> IO ()
handleCommand (UI.AddDrill filePath) conn = drillFromFile filePath conn
handleCommand UI.Review conn = review conn
handleCommand UI.Status conn = status conn

drillFromFile :: FilePath -> Connection -> IO ()
drillFromFile filePath conn = do
    withFile filePath ReadMode processFile
    where
        processFile handle = do
            contents <- TIO.hGetContents handle
            let drill = Drill {drillId = 0, drillFileName = filePath, drillBody = contents}
            execute conn Queries.insertDrillQuery drill
            rowId <- lastInsertRowId conn
            let grade = initGrade (drill {drillId = fromIntegral rowId})
            execute conn Queries.insertGradeQuery grade
            putStrLn ("Added drill for " <> filePath)

getGrades :: Connection -> IO [Grade]
getGrades conn = do
    grades <- query_ conn Queries.getDueQuery
    return $ map toGrade grades

toGrade :: (Int, Int, Int, Float, FilePath, Text.Text, Int) -> Grade
toGrade (gradeId', drillId', streak, score, filename, body, interval) =
    Grade
        { gradeId = gradeId'
        , gradeStreak = streak
        , gradeScore = score
        , gradeDrill = Drill {drillId = drillId', drillFileName = filename, drillBody = body}
        , gradeInterval = interval
        , gradeLastReviewed = ModifiedJulianDay 0
        }

review :: Connection -> IO ()
review conn = do
    grades <- getGrades conn
    review' grades
    where
        review' [] = putStrLn "Nothing left to review."
        review' (grade' : _rest) = do
            let drill' = gradeDrill grade'
            let filename = "/tmp/" <> drillFileName drill'
            TIO.writeFile filename (drillBody drill')
            callCommand $ vimCommand filename
            newScore <- UI.getScore
            currentDay <- getCurrentDay
            let newGrade' = applySM2Grade grade' (SM2.applyScore (fromIntegral newScore) (toSM2Grade grade'))
            let newGrade = newGrade' {gradeLastReviewed = currentDay}
            execute conn Queries.updateGradeQuery (gradeStreak newGrade, gradeScore newGrade, gradeInterval newGrade, gradeLastReviewed newGrade, gradeId newGrade)
            grades' <- getGrades conn
            continue grades'
        continue [] = review' []
        continue grades' = do
            willContinue <- UI.getContinue
            when willContinue $ review' grades'

toSM2Grade :: Grade -> SM2.Grade
toSM2Grade grade =
    SM2.Grade (gradeStreak grade) (gradeScore grade) (gradeInterval grade)

applySM2Grade :: Grade -> SM2.Grade -> Grade
applySM2Grade grade (SM2.Grade streak score interval) =
    grade {gradeStreak = streak, gradeScore = score, gradeInterval = interval}

getCurrentDay :: IO Day
getCurrentDay = do
    utctDay <$> getCurrentTime

vimCommand :: FilePath -> String
vimCommand filePath = "vim " <> filePath

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
