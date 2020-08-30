CREATE TABLE user
(
    id              INTEGER NOT NULL PRIMARY KEY,
    first_name      TEXT NOT NULL,
    username        TEXT NOT NULL,
    status          INTEGER NOT NULL,
    participant     INTEGER NOT NULL
);
CREATE TABLE state
(
    description     TEXT,
    longitude       REAL NOT NULL,
    latitude        REAL NOT NULL,
    datetime        TEXT NOT NULL,
    past            INTEGER NOT NULL
);
INSERT INTO state   VALUES (NULL,0.0,0.0, '2020-08-25T23:25:38.988+03:00[UTC+03:00]', true);
INSERT INTO user    VALUES (125590757, 'Igor', 'igorramazanov', 4);
INSERT INTO user    VALUES (381877311, 'Liza', 'ohlouisa', 1);