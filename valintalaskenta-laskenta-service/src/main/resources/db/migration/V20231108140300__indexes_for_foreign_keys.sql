CREATE INDEX IF NOT EXISTS jonosija_valintatapajono ON jonosija(valintatapajono);

CREATE INDEX IF NOT EXISTS jonosija_hakijaryhma ON jonosija(hakijaryhma);

CREATE INDEX IF NOT EXISTS muokattu_jonosija_valintatapajono ON muokattu_jonosija(valintatapajono_oid);

CREATE INDEX IF NOT EXISTS valintatapajono_valinnanvaihe on valintatapajono(valinnanvaihe);

CREATE INDEX IF NOT EXISTS valintakoe_valintakoe_valinnanvaihe ON valintakoe(valintakoe_valinnanvaihe);

CREATE INDEX IF NOT EXISTS valintakoe_valinnanvaihe_hakutoive on valintakoe_valinnanvaihe(hakutoive);

CREATE INDEX IF NOT EXISTS hakutoive_valintakoe_osallistuminen on hakutoive(valintakoe_osallistuminen);


