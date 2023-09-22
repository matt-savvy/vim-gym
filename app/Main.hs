module Main (main) where
import System.Environment (getArgs)
import System.IO (withFile, IOMode(..), hGetContents)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Process
import Database.SQLite.Simple (Connection, withConnection, Query, execute)
import Database.SQLite.Simple.ToRow (toRow, ToRow)
import GHC.Generics (Generic)
import Data.Time (Day(..))

data Command = AddDrill { path :: FilePath } | Review deriving (Show)

data Drill = Drill { drillId :: Int, fileName :: FilePath, body :: Text.Text } deriving (Show)
instance ToRow Drill where
    toRow (Drill _id fileName body) = toRow (fileName, body)

data Grade = Grade { gradeId :: Int, drill :: Drill, streak :: Int, score :: Float, interval :: Int, lastReviewed :: Day }

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
handleCommand Review _conn = review

drillFromFile :: FilePath -> Connection -> IO ()
drillFromFile filePath conn = do
    withFile filePath ReadMode processFile
    where processFile handle = do
              contents <- TIO.hGetContents handle
              let drill = Drill { drillId = 0, fileName = filePath, body = contents }
              let grade = initGrade drill
              execute conn insertDrillQuery drill
              execute conn insertGradeQuery grade

insertDrillQuery :: Query
insertDrillQuery = "INSERT INTO drills (filename, body) VALUES (?, ?)"

insertGradeQuery :: Query
insertGradeQuery = "INSERT INTO grades (drill_id, streak, score, interval, last_reviewed) VALUES (?, ?, ?, ?, ?)"


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
    withConnection "vim-gym.db" $ \conn -> do
        handleCommand command conn
        print ""
