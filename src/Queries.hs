module Queries where
import Database.SQLite.Simple

getDueCountQuery :: Query
getDueCountQuery = "SELECT COUNT (id) FROM grades WHERE datetime(last_reviewed, '+' || interval || ' day') <= datetime('now');"
