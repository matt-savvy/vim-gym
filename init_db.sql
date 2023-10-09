CREATE TABLE drills (
    id INTEGER PRIMARY KEY,
    streak INTEGER,
    score REAL,
    interval INTEGER,
    last_reviewed TEXT
);

CREATE TABLE files (
    id INTEGER PRIMARY KEY,
    filename TEXT,
    body TEXT,
    drill_id INTEGER,
    FOREIGN KEY(drill_id) REFERENCES drills(id)
);
