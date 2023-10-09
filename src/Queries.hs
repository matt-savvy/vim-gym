module Queries (
    getDueCountQuery,
    getDueQuery,
    getFilesQuery,
    insertDrillQuery,
    insertFileQuery,
    insertGradeQuery,
    updateGradeQuery,
) where

import Database.SQLite.Simple (Query)

getDueCountQuery :: Query
getDueCountQuery = "SELECT COUNT (id) FROM grades WHERE datetime(last_reviewed, '+' || interval || ' day') <= datetime('now');"

getDueQuery :: Query
getDueQuery = "SELECT grades.id, drill_id, streak, score, filename, body, interval FROM grades JOIN drills ON grades.drill_id = drills.id WHERE datetime(last_reviewed, '+' || interval || ' day') <= datetime('now') LIMIT 1;"

getFilesQuery :: Query
getFilesQuery = "SELECT drill_id, filename, body FROM files WHERE drill_id = (?);"

insertDrillQuery :: Query
insertDrillQuery = "INSERT INTO drills (filename, body) VALUES (?, ?)"

insertGradeQuery :: Query
insertGradeQuery = "INSERT INTO grades (drill_id, streak, score, interval, last_reviewed) VALUES (?, ?, ?, ?, ?)"

insertFileQuery :: Query
insertFileQuery = "INSERT INTO files (drill_id, filename, body) VALUES (?, ?, ?)"

updateGradeQuery :: Query
updateGradeQuery = "UPDATE grades SET streak = ?, score = ?, interval = ?, last_reviewed = ? WHERE id = ?"
