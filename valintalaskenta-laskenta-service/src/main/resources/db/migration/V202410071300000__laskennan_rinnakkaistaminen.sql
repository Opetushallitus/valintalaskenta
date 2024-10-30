ALTER TABLE seuranta_laskenta_hakukohteet ADD COLUMN yritykset INTEGER DEFAULT 0;
ALTER TABLE seuranta_laskenta_hakukohteet ADD COLUMN noodi_id TEXT;

CREATE TABLE IF NOT EXISTS noodit (
                                      noodi_id  TEXT PRIMARY KEY,
                                      alive     TIMESTAMP
);

CREATE TABLE IF NOT EXISTS parametrit (
                                      nimi  TEXT PRIMARY KEY,
                                      arvo  TEXT NOT NULL
);

INSERT INTO parametrit (nimi, arvo) VALUES ('maxYhtaaikaisetHakukohteet', '8');