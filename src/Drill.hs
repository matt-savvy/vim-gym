module Drill (Drill (..), initDrill) where

import Data.Time (Day (..))
import Database.SQLite.Simple

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
