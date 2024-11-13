-- tämä on jälkitöitä seurannan siirrosta
UPDATE seuranta_laskennat SET valintakoelaskenta=false WHERE valintakoelaskenta IS NULL;
ALTER TABLE seuranta_laskennat ALTER COLUMN valintakoelaskenta SET NOT NULL;

ALTER TABLE seuranta_laskenta_hakukohteet ADD COLUMN IF NOT EXISTS yritykset INTEGER DEFAULT 0;
ALTER TABLE seuranta_laskenta_hakukohteet ADD COLUMN IF NOT EXISTS noodi_id TEXT;
ALTER TABLE seuranta_laskenta_hakukohteet ADD COLUMN IF NOT EXISTS luotu TIMESTAMP;
ALTER TABLE seuranta_laskenta_hakukohteet ADD COLUMN IF NOT EXISTS aloitettu TIMESTAMP;
ALTER TABLE seuranta_laskenta_hakukohteet ADD COLUMN IF NOT EXISTS lopetettu TIMESTAMP;

CREATE TABLE IF NOT EXISTS noodit (
                                      noodi_id  TEXT PRIMARY KEY,
                                      alive     TIMESTAMP
);

CREATE TABLE IF NOT EXISTS parametrit (
                                      nimi  TEXT PRIMARY KEY,
                                      arvo  TEXT NOT NULL
);

INSERT INTO parametrit (nimi, arvo) VALUES ('maxYhtaaikaisetHakukohteet', '8') ON CONFLICT DO NOTHING;