ALTER TABLE harkinnanvarainen_hyvaksyminen_history ADD COLUMN system_time tstzrange not null default tstzrange(now(), null, '[)');
ALTER TABLE hakijaryhma_history ADD COLUMN system_time tstzrange not null default tstzrange(now(), null, '[)');
ALTER TABLE valinnanvaihe_history ADD COLUMN system_time tstzrange not null default tstzrange(now(), null, '[)');
ALTER TABLE valintakoe_osallistuminen_history ADD COLUMN system_time tstzrange not null default tstzrange(now(), null, '[)');
ALTER TABLE hakutoive_history ADD COLUMN system_time tstzrange not null default tstzrange(now(), null, '[)');
ALTER TABLE valintakoe_valinnanvaihe_history ADD COLUMN system_time tstzrange not null default tstzrange(now(), null, '[)');
ALTER TABLE valintakoe_history ADD COLUMN system_time tstzrange not null default tstzrange(now(), null, '[)');
ALTER TABLE valintatapajono_history ADD COLUMN system_time tstzrange not null default tstzrange(now(), null, '[)');
ALTER TABLE jonosija_history ADD COLUMN system_time tstzrange not null default tstzrange(now(), null, '[)');
ALTER TABLE muokattu_jonosija_history ADD COLUMN system_time tstzrange not null default tstzrange(now(), null, '[)');
ALTER TABLE jarjestyskriteeritulos_history ADD COLUMN system_time tstzrange not null default tstzrange(now(), null, '[)');
