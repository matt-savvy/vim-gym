module Queries (
    getDueCountQuery,
    getDueQuery,
    getFilesQuery,
    insertFileQuery,
    insertDrillQuery,
    listDrillsQuery,
    updateDrillQuery,
) where

import Database.SQLite.Simple (Query)

getDueCountQuery :: Query
getDueCountQuery = "SELECT COUNT (id) FROM drills WHERE datetime(last_reviewed, '+' || interval || ' day') <= datetime('now');"

getDueQuery :: Query
getDueQuery = "SELECT id, streak, score, interval, last_reviewed FROM drills WHERE datetime(last_reviewed, '+' || interval || ' day') <= datetime('now') LIMIT 1;"

listDrillsQuery :: Query
listDrillsQuery = "SELECT id, streak, score, interval, last_reviewed FROM drills;"

getFilesQuery :: Query
getFilesQuery = "SELECT drill_id, filename, body FROM files WHERE drill_id = (?);"

insertDrillQuery :: Query
insertDrillQuery = "INSERT INTO drills (streak, score, interval, last_reviewed) VALUES (?, ?, ?, ?)"

insertFileQuery :: Query
insertFileQuery = "INSERT INTO files (drill_id, filename, body) VALUES (?, ?, ?)"

updateDrillQuery :: Query
updateDrillQuery = "UPDATE drills SET streak = ?, score = ?, interval = ?, last_reviewed = ? WHERE id = ?"
