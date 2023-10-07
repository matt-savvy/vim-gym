module Queries (getDueCountQuery) where
import Database.SQLite.Simple (Query)

getDueCountQuery :: Query
getDueCountQuery = "SELECT COUNT (id) FROM grades WHERE datetime(last_reviewed, '+' || interval || ' day') <= datetime('now');"
