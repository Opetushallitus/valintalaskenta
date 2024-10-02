ALTER TABLE seuranta_laskenta_hakukohteet ADD COLUMN yritykset INTEGER DEFAULT 0;
ALTER TABLE seuranta_laskenta_hakukohteet ADD COLUMN noodi_id TEXT;

CREATE TABLE IF NOT EXISTS noodit (
                                      noodi_id  TEXT PRIMARY KEY,
                                      alive     TIMESTAMP
);