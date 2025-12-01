DO $$ BEGIN
    CREATE TYPE osallistumistieto AS ENUM (
        'EI_OSALLISTUNUT',
        'OSALLISTUI',
        'MERKITSEMATTA',
        'EI_VAADITA'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;


CREATE TABLE IF NOT EXISTS valintapiste (
    hakemus_oid varchar not null,
    tunniste varchar not null,
    arvo varchar,
    osallistuminen osallistumistieto,
    tallettaja varchar not null,
    system_time tstzrange not null default tstzrange(now(), null, '[)'),
    transaction_id bigint not null default txid_current(),
    primary key (hakemus_oid, tunniste)
);

CREATE TABLE IF NOT EXISTS valintapiste_history (like valintapiste);

DROP FUNCTION IF EXISTS update_valintapiste_history;
CREATE FUNCTION update_valintapiste_history() RETURNS TRIGGER
    LANGUAGE plpgsql
AS $$
BEGIN
INSERT INTO valintapiste_history (
    hakemus_oid,
    tunniste,
    arvo,
    osallistuminen,
    tallettaja,
    system_time,
    transaction_id
) VALUES (
    old.hakemus_oid,
    old.tunniste,
    old.arvo,
    old.osallistuminen,
    old.tallettaja,
    tstzrange(lower(old.system_time), now(), '[)'),
    old.transaction_id
);
RETURN null;
END;
$$;

CREATE OR REPLACE TRIGGER delete_valintapiste_history
    AFTER DELETE
    ON valintapiste
    FOR EACH ROW
    EXECUTE PROCEDURE update_valintapiste_history();

CREATE OR REPLACE TRIGGER update_valintapiste_history
    AFTER UPDATE
    ON valintapiste
    FOR EACH ROW
    EXECUTE PROCEDURE update_valintapiste_history();

CREATE INDEX IF NOT EXISTS valintapiste_modified_at_idx ON valintapiste (lower(system_time));
CREATE INDEX IF NOT EXISTS valintapiste_history_modified_at_idx ON valintapiste_history (upper(system_time));

COMMENT ON TABLE valintapiste is 'Hakijan valintakokeiden osallistumis- ja pistetiedot';
COMMENT ON COLUMN valintapiste.hakemus_oid is 'Hakemuksen OID';
COMMENT ON COLUMN valintapiste.tunniste is 'Valintakokeen tunniste (syötettävä arvo)';
COMMENT ON COLUMN valintapiste.arvo is 'Valintakokeen pisteet';
COMMENT ON COLUMN valintapiste.osallistuminen is 'Valintakokeen osallistumistieto';
COMMENT ON COLUMN valintapiste.tallettaja is 'Tallentajan OID';
COMMENT ON COLUMN valintapiste.system_time is 'Tallennuksen aikaleima';
COMMENT ON COLUMN valintapiste.transaction_id is 'Tallennuksen transaktio-id';
