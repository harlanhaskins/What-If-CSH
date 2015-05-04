DROP DATABASE IF EXISTS whatifcsh;
DROP ROLE IF EXISTS whatifcsh;

CREATE DATABASE whatifcsh OWNER whatifcsh;

\connect whatifcsh;

DROP TABLE IF EXISTS suggestions;

CREATE TABLE suggestions (
    description VARCHAR(255) UNIQUE NOT NULL
);

ALTER TABLE suggestions OWNER TO whatifcsh;
