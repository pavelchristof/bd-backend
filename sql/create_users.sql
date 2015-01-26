CREATE TABLE IF NOT EXISTS Users (
    id serial PRIMARY KEY,
    username varchar(256) UNIQUE,
    password varchar(256)
);
