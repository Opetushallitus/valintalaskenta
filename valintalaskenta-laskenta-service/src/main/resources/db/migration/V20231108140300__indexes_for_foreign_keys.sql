CREATE INDEX jonosija_valintatapajono ON jonosija(valintatapajono);

CREATE INDEX jonosija_hakijaryhma ON jonosija(hakijaryhma);

CREATE INDEX muokattu_jonosija_valintatapajono ON muokattu_jonosija(valintatapajono_oid);

CREATE INDEX jarjestyskriteeritulos_jonosija on jarjestyskriteeritulos(jonosija);

CREATE INDEX jarjestyskriteeritulos_muokattu_jonosija on jarjestyskriteeritulos(muokattu_jonosija);

CREATE INDEX valintatapajono_valinnanvaihe on valintatapajono(valinnanvaihe);

CREATE INDEX valintakoe_valintakoe_valinnanvaihe ON valintakoe(valintakoe_valinnanvaihe);

CREATE INDEX valintakoe_valinnanvaihe_hakutoive on valintakoe_valinnanvaihe(hakutoive);

CREATE INDEX hakutoive_valintakoe_osallistuminen on hakutoive(valintakoe_osallistuminen);


