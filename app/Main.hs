module Main (main) where
import qualified SM2
import System.Environment (getArgs)
import System.IO (withFile, IOMode(..), hGetContents)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Process
import Database.SQLite.Simple
import Data.Time (Day(..), getCurrentTime, utctDay)

data Command = AddDrill { path :: FilePath } | Review deriving (Show)

data Drill = Drill { drillId :: Int, fileName :: FilePath, body :: Text.Text } deriving (Show)

instance ToRow Drill where
    toRow (Drill _id fileName body) = toRow (fileName, body)


data Grade = Grade { gradeId :: Int, drill :: Drill, streak :: Int, score :: Float, interval :: Int, lastReviewed :: Day } deriving Show

instance ToRow Grade where
    toRow (Grade _id drill streak score interval lastReviewed) = toRow ( (drillId drill), streak, score, interval, lastReviewed)

initGrade :: Drill -> Grade
initGrade drill = Grade
    { gradeId = 0
    , drill = drill
    , streak = 0
    , score = 2.5
    , interval = 1
    , lastReviewed = (ModifiedJulianDay 0)
    }

handleArgs :: [String] -> Command
handleArgs ["add", filePath] = AddDrill { path = filePath }
handleArgs ["review"] = Review
handleArgs _args = undefined

handleCommand :: Command -> Connection -> IO ()
handleCommand (AddDrill filePath) conn = drillFromFile filePath conn
handleCommand Review conn = review conn

drillFromFile :: FilePath -> Connection -> IO ()
drillFromFile filePath conn = do
    withFile filePath ReadMode processFile
    where processFile handle = do
              contents <- TIO.hGetContents handle
              let drill = Drill { drillId = 0, fileName = filePath, body = contents }
              execute conn insertDrillQuery drill
              rowId <- lastInsertRowId conn
              let grade = initGrade (drill {drillId = (fromIntegral rowId)})
              execute conn insertGradeQuery grade

insertDrillQuery :: Query
insertDrillQuery = "INSERT INTO drills (filename, body) VALUES (?, ?)"

insertGradeQuery :: Query
insertGradeQuery = "INSERT INTO grades (drill_id, streak, score, interval, last_reviewed) VALUES (?, ?, ?, ?, ?)"

updateGradeQuery :: Query
updateGradeQuery = "UPDATE grades SET streak = ?, score = ?, interval = ?, last_reviewed = ? WHERE id = ?"

getDueQuery :: Query
getDueQuery = "SELECT grades.id, drill_id, streak, score, filename, body, interval FROM grades JOIN drills ON grades.drill_id = drills.id WHERE datetime(last_reviewed, '+' || interval || ' day') <= datetime('now') LIMIT 1;"

getGrades :: Connection -> IO [Grade]
getGrades conn = do
    grades <- query_ conn getDueQuery
    return $ map toGrade grades

toGrade :: (Int, Int, Int, Float, FilePath, Text.Text, Int) -> Grade
toGrade (gradeId, drillId, streak, score, filename, body, interval) =
    Grade { gradeId = gradeId
          , streak = streak
          , score = score
          , drill = Drill { drillId = drillId, fileName = filename, body = body }
          , interval = interval
          , lastReviewed = (ModifiedJulianDay 0)
          }


review :: Connection -> IO ()
review conn = do
    grades <- getGrades conn
    case grades of
        [] -> putStrLn "all done"
        (grade : _rest) -> do
            let drill' = drill grade
            let filename = "/tmp/" <> (fileName drill')
            TIO.writeFile filename (body drill')
            callCommand $ vimCommand filename
            newScore <- getScore
            currentDay <- getCurrentDay
            let newGrade' = applySM2Grade grade (SM2.applyScore newScore (toSM2Grade grade))
            let newGrade = newGrade' { lastReviewed = currentDay }
            execute conn updateGradeQuery (streak newGrade, score newGrade, interval newGrade, lastReviewed newGrade, gradeId newGrade)


toSM2Grade :: Grade -> SM2.Grade
toSM2Grade grade =
    SM2.Grade (streak grade) (score grade) (interval grade)

applySM2Grade :: Grade -> SM2.Grade -> Grade
applySM2Grade grade (SM2.Grade streak score interval) =
    grade { streak = streak, score = score, interval = interval}

getScore :: IO Float
getScore = do
    putStrLn "how did you do? score from 0 - 5"
    line <- getLine
    let newScore = read line
    return newScore

getCurrentDay :: IO Day
getCurrentDay = do
    utcTime <- getCurrentTime
    return $ utctDay utcTime

vimCommand :: FilePath -> String
vimCommand filePath = "/home/mattsavoia/.nix-profile/bin/vim" <> " " <> filePath

main :: IO ()
main = do
    args <- getArgs
    let command = handleArgs args
    withConnection "vim-gym.db" $ \conn -> do
        handleCommand command conn
        print ""
