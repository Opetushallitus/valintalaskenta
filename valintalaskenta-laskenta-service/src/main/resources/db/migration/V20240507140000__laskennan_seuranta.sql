CREATE TABLE IF NOT EXISTS seuranta_laskennat (
    uuid        UUID NOT NULL PRIMARY KEY,
    haunnimi    TEXT NOT NULL,
    nimi        TEXT,
    hakuoid     TEXT NOT NULL,
    luotu       TIMESTAMP NOT NULL,
    aloitettu   TIMESTAMP,
    lopetettu   TIMESTAMP,
    tila        TEXT NOT NULL,
    tyyppi      TEXT NOT NULL,
    valinnanvaihe INTEGER,
    valintakoelaskenta BOOLEAN,
    erillishaku BOOLEAN NOT NULL,
    useroid TEXT NOT NULL,
    identityhash TEXT NOT NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS seuranta_laskennat_identityhash_unique ON seuranta_laskennat (identityhash) WHERE (tila='ALOITTAMATTA' OR tila='MENEILLAAN');

CREATE TABLE IF NOT EXISTS seuranta_laskenta_hakukohteet (
    laskenta_uuid UUID NOT NULL,
    hakukohdeoid TEXT NOT NULL,
    organisaatiooid TEXT NOT NULL,
    tila TEXT NOT NULL,
    PRIMARY KEY (laskenta_uuid, hakukohdeoid),
    FOREIGN KEY (laskenta_uuid) REFERENCES seuranta_laskennat (uuid) ON DELETE CASCADE
);

-- hakukohteiden ja laskentojen ilmoitukset, ilmoitus laskennan jos hakukohdeoid==null
CREATE TABLE IF NOT EXISTS seuranta_hakukohde_ilmoitukset (
    laskenta_uuid UUID NOT NULL,
    hakukohdeoid TEXT,
    ilmoitustyyppi TEXT NOT NULL,
    otsikko TEXT NOT NULL,
    luotu TIMESTAMP NOT NULL,
    data TEXT[],
    FOREIGN KEY (laskenta_uuid, hakukohdeoid) REFERENCES seuranta_laskenta_hakukohteet (laskenta_uuid, hakukohdeoid) MATCH SIMPLE ON DELETE CASCADE
);

-- hakukohteilla voi olla monta ilmoitusta, mutta laskennalla vain yksi
CREATE UNIQUE INDEX IF NOT EXISTS seuranta_hakukohde_ilmoitukset_uuid_unique ON seuranta_hakukohde_ilmoitukset (laskenta_uuid) WHERE (hakukohdeoid IS null);
