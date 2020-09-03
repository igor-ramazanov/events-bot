CREATE TABLE user
(
    id          INT NOT NULL PRIMARY KEY,
    first_name  TEXT NOT NULL,
    username    TEXT NOT NULL,
    status      TEXT NOT NULL,
    participant INT NOT NULL
);

CREATE TABLE event
(
    description     TEXT,
    longitude       REAL NOT NULL,
    latitude        REAL NOT NULL,
    datetime        TEXT NOT NULL,
    past            INT NOT NULL,
    needs_reminding INT NOT NULL
);

CREATE TABLE garden
(
    last_watering   TEXT NOT NULL,
    next_watering   TEXT NOT NULL
);

INSERT INTO event   VALUES (NULL, 0.0, 0.0, '2020-08-25T23:25:38.988+03:00[UTC+03:00]', 1, 0);
INSERT INTO user    VALUES (125590757, 'Igor', 'igorramazanov', 'Admin', 0, 0);
INSERT INTO user    VALUES (381877311, 'Liza', 'ohlouisa', 'SignedIn', 0, 0);