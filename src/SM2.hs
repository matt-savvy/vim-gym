module SM2 (Grade(..), applyScore) where

data Grade = Grade { streak :: Int, score :: Float, interval :: Int } deriving (Show, Eq)

applyScore :: Float -> Grade -> Grade
applyScore newScore grade
    | newScore >= 3.0 = correct newScore grade
    | otherwise = incorrect newScore grade

correct :: Float -> Grade -> Grade
correct newScore grade@(Grade 0 currentScore _interval) =
    succReps (grade { interval = 1, score = (nextScore newScore currentScore) })
correct newScore grade@(Grade 1 currentScore _interval) =
    succReps (grade { interval = 6, score = (nextScore newScore currentScore) })
correct newScore grade@(Grade _streak currentScore interval') =
    succReps (grade { interval = (round ((fromIntegral interval') * newScore)), score = (nextScore newScore currentScore) })

incorrect :: Float -> Grade -> Grade
incorrect newScore grade = grade { streak = 0, interval = 1, score = (nextScore newScore currentScore) }
    where currentScore = score grade

succReps :: Grade -> Grade
succReps grade = grade { streak = (succ . streak) grade }

nextScore :: Float -> Float -> Float
nextScore newScore currentScore =
    let newScoreFactor = 5 - newScore
        nextScore' = currentScore + (0.1 - newScoreFactor * (0.08 + newScoreFactor * 0.02))
    in max 1.3 nextScore'
