\connect harlan_whatifcsh;

DROP TABLE IF EXISTS suggestions;

CREATE TABLE suggestions (
    description VARCHAR(255) UNIQUE NOT NULL
);

ALTER TABLE suggestions OWNER TO harlan_whatifcsh;
