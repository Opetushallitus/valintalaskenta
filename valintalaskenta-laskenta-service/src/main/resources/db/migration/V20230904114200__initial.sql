CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE harkinnanvarainen_hyvaksyminen (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at timestamp with time zone default now(),
    harkinnanvaraisuus_tila varchar(255),
    hakukohde_oid varchar(127) NOT NULL,
    hakemus_oid varchar(127) NOT NULL,
    haku_oid varchar(127) NOT NULL
);

CREATE TABLE valinnanvaihe (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    jarjestysnumero int,
    created_at timestamp with time zone default now(),
    haku_oid varchar(127) NOT NULL,
    hakukohde_oid varchar(127) NOT NULL,
    valinnanvaihe_oid varchar(127) UNIQUE NOT NULL,
    tarjoaja_oid varchar(127) NOT NULL,
    nimi varchar(255)
);

CREATE INDEX valinnanvaihe_hakukohde ON valinnanvaihe(hakukohde_oid);
CREATE INDEX valinnanvaihe_haku ON valinnanvaihe(haku_oid);

CREATE TABLE valintakoe_osallistuminen (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    haku_oid varchar(127) NOT NULL,
    hakemus_oid varchar(127) UNIQUE,
    hakija_oid varchar(127) NOT NULL,
    etunimi varchar(255),
    sukunimi varchar(255),
    created_at timestamp with time zone default now()
);

CREATE INDEX valintakoeosallistuminen_haku ON valintakoe_osallistuminen(haku_oid);
CREATE INDEX valintakoeosallistuminen_hakija ON valintakoe_osallistuminen(hakija_oid);
CREATE INDEX valintakoeosallistuminen_hakemus ON valintakoe_osallistuminen(hakemus_oid);

CREATE TABLE hakutoive (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    hakukohde_oid varchar(127) NOT NULL,
    valintakoe_osallistuminen uuid,
    CONSTRAINT fk_valintakoeosallistuminen
        FOREIGN KEY(valintakoe_osallistuminen)
            REFERENCES valintakoe_osallistuminen(id)
            ON DELETE CASCADE
);

CREATE INDEX hakutoive_hakukohde ON hakutoive(hakukohde_oid);

CREATE TABLE valintakoe_valinnanvaihe (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    valinnanvaihe_oid varchar(127) NOT NULL,
    valinnan_vaihe_jarjestysluku int,
    hakutoive uuid not null,
    CONSTRAINT fk_hakutoive
        FOREIGN KEY(hakutoive)
            REFERENCES Hakutoive(id)
);

CREATE TABLE valintakoe (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    valintakoe_oid varchar(127) NOT NULL,
    valintakoe_tunniste varchar(127),
    nimi varchar(255),
    aktiivinen boolean DEFAULT NULL,
    valintakoe_valinnanvaihe uuid,
    lahetetaanko_koekutsut boolean DEFAULT NULL,
    osallistuminen varchar(255),
    kutsuttavien_maara int,
    kutsun_kohde varchar(255),
    kutsun_kohde_avain varchar(255),
    kuvaus_fi text,
    kuvaus_sv text,
    kuvaus_en text,
    laskenta_tila varchar(255),
    laskenta_tulos boolean,
    tekninen_kuvaus text,
    CONSTRAINT fk_valinnanvaihe
        FOREIGN KEY(valintakoe_valinnanvaihe)
            REFERENCES valintakoe_valinnanVaihe(id)
);

CREATE TABLE valintatapajono (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at timestamp with time zone default now(),
    valintatapajono_oid varchar(127) UNIQUE,
    nimi varchar(255),
    prioriteetti int,
    aloituspaikat int,
    siirretaan_sijoitteluun boolean DEFAULT NULL,
    tasasijasaanto varchar(100),
    ei_varasijatayttoa boolean DEFAULT NULL,
    kaikki_ehdon_tayttavat_hyvaksytaan boolean DEFAULT NULL,
    kaytetaan_valintalaskentaa boolean DEFAULT NULL,
    valmis_sijoiteltavaksi boolean DEFAULT NULL,
    kaytetaan_kokonaispisteita boolean DEFAULT NULL,
    valinnanvaihe uuid NOT NULL,
    valinnanvaihe_key int,
    sijoitteluajo_id int,
    poissa_oleva_taytto boolean DEFAULT NULL,
    CONSTRAINT fk_valinnanvaihe
        FOREIGN KEY(valinnanvaihe)
            REFERENCES valinnanvaihe(id)
);

CREATE TABLE hakijaryhma (
     id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
     hakijaryhma_oid varchar(127) UNIQUE,
     prioriteetti int,
     created_at timestamp with time zone default now(),
     hakukohde_oid varchar(127),
     nimi varchar(255),
     kuvaus text,
     kiintio int,
     kayta_kaikki boolean DEFAULT NULL,
     tarkka_kiintio boolean DEFAULT NULL,
     kaytetaan_ryhmaan_kuuluvia boolean DEFAULT NULL,
     hakijaryhmatyyppi_koodiuri varchar(255),
     valintatapajono_oid varchar(127) DEFAULT NULL
);

CREATE INDEX hakijaryhma_hakukohde ON hakijaryhma(hakukohde_oid);

CREATE TABLE jonosija (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at timestamp with time zone default now(),
    etunimi varchar(255),
    sukunimi varchar(255),
    hakemus_oid varchar(127),
    hakija_oid varchar(127),
    hakutoiveprioriteetti int,
    harkinnanvarainen boolean DEFAULT NULL,
    hylatty_valisijoittelussa boolean DEFAULT NULL,
    valintatapajono uuid,
    hakijaryhma uuid,
    funktio_tulokset jsonb,
    syotetyt_arvot jsonb,
    CONSTRAINT fk_valintatapajono
        FOREIGN KEY(valintatapajono)
            REFERENCES valintatapajono(id),
    CONSTRAINT fk_hakijaryhma
        FOREIGN KEY(hakijaryhma)
            REFERENCES hakijaryhma(id)
);

CREATE INDEX jonosija_hakemus ON jonosija(hakemus_oid);

CREATE TABLE muokattu_jonosija (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at timestamp with time zone default now(),
    hakukohde_oid varchar(127) NOT NULL,
    haku_oid varchar(127) NOT NULL,
    valintatapajono_oid varchar(127) NOT NULL,
    hakemus_oid varchar(127) NOT NULL,
    harkinnanvarainen boolean DEFAULT NULL,
    prioriteetti int,
    selite varchar(255),
    muutos varchar(255),
    CONSTRAINT fk_valintatapajono_oid
      FOREIGN KEY(valintatapajono_oid)
          REFERENCES Valintatapajono(valintatapajono_oid)
);

CREATE INDEX muokattujonosija_hakukohde ON muokattu_jonosija(hakukohde_oid);
CREATE INDEX muokattujonosija_haku ON muokattu_jonosija(haku_oid);

CREATE TABLE jarjestyskriteeritulos (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at timestamp with time zone default now(),
    prioriteetti int,
    arvo varchar(255),
    tila varchar(100),
    nimi varchar(255),
    kuvaus_fi text NOT NULL DEFAULT '',
    kuvaus_sv text NOT NULL DEFAULT '',
    kuvaus_en text NOT NULL DEFAULT '',
    tekninen_kuvaus text,
    jonosija uuid,
    muokattu_jonosija uuid,
    CONSTRAINT fk_jonosija
        FOREIGN KEY(jonosija)
            REFERENCES jonosija(id),
    CONSTRAINT fk_muokattujonosija
        FOREIGN KEY(muokattu_jonosija)
            REFERENCES muokattu_jonosija(id)
);




