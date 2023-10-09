module Queries (
    getDueCountQuery,
    getDueQuery,
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

insertDrillQuery :: Query
insertDrillQuery = "INSERT INTO drills (filename, body) VALUES (?, ?)"

insertGradeQuery :: Query
insertGradeQuery = "INSERT INTO grades (drill_id, streak, score, interval, last_reviewed) VALUES (?, ?, ?, ?, ?)"

insertFileQuery :: Query
insertFileQuery = "INSERT INTO files (drill_id, filename, body) VALUES (?, ?, ?)"

updateGradeQuery :: Query
updateGradeQuery = "UPDATE grades SET streak = ?, score = ?, interval = ?, last_reviewed = ? WHERE id = ?"
