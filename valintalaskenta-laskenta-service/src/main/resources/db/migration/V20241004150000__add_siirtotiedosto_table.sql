CREATE TABLE IF NOT EXISTS siirtotiedosto
(
    execution_uuid varchar,
    window_start   timestamp with time zone not null,
    window_end     timestamp with time zone not null not null,
    run_start      timestamp with time zone not null default now(),
    run_end        timestamp with time zone,
    info           jsonb,
    success        boolean,
    error_message  varchar,
    PRIMARY KEY (execution_uuid)
);

COMMENT ON COLUMN siirtotiedosto.execution_uuid IS 'Operaation tunniste (uuid)';
COMMENT ON COLUMN siirtotiedosto.window_start IS 'Siirtotiedosto-operaation aikaikkunan alkuaika (siirtotiedostoon tulevat tällä aikavälillä muuttuneet valintalaskennan tulokset)';
COMMENT ON COLUMN siirtotiedosto.window_end IS 'Siirtotiedosto-operaation aikaikkunan loppuaika (siirtotiedostoon tulevat tällä aikavälillä muuttuneet valintalaskennan tulokset)';
COMMENT ON COLUMN siirtotiedosto.run_start IS 'Siirtotiedosto-operaation suorituksen alkuaika';
COMMENT ON COLUMN siirtotiedosto.run_end IS 'Siirtotiedosto-operaation suorituksen loppuaika';
COMMENT ON COLUMN siirtotiedosto.info IS 'Tietoja tallennetuista entiteeteistä, mm. lukumäärät';
COMMENT ON COLUMN siirtotiedosto.error_message IS 'null, jos mikään ei mennyt vikaan';

--These initial values expect that data before the hardcoded first window_end will be handled manually through swagger or similar.
INSERT INTO siirtotiedosto(execution_uuid, window_start, window_end, run_start, run_end, info, success, error_message)
VALUES ('23be1612-be52-419f-a93e-d03245f1s62g', '1970-01-01 00:00:00.000000 +00:00', '2024-08-01 00:00:00.000000 +00:00', now(), now(), '{}'::jsonb, true, null) ON CONFLICT DO NOTHING;

