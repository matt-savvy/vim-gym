CREATE TABLE drills (id INTEGER PRIMARY KEY, filename TEXT, body TEXT);
CREATE TABLE grades (id INTEGER PRIMARY KEY, drill_id INTEGER, streak INTEGER, score REAL, interval INTEGER, last_reviewed TEXT);
