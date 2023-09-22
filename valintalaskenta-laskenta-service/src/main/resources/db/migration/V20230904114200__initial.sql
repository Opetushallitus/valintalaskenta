CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE HarkinnanvarainenHyvaksyminen (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at timestamp with time zone default now(),
    harkinnanvaraisuustila varchar(255),
    hakukohde_oid varchar(127) NOT NULL,
    hakemus_oid varchar(127) NOT NULL,
    haku_oid varchar(127) NOT NULL
);

CREATE TABLE Valinnanvaihe (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    jarjestysnumero int,
    created_at timestamp with time zone default now(),
    haku_oid varchar(127) NOT NULL,
    hakukohde_oid varchar(127) NOT NULL,
    valinnanvaihe_oid varchar(127) UNIQUE NOT NULL,
    tarjoaja_oid varchar(127) NOT NULL,
    nimi varchar(255)
);

CREATE INDEX valinnanvaihe_hakukohde ON Valinnanvaihe(hakukohde_oid);
CREATE INDEX valinnanvaihe_haku ON Valinnanvaihe(haku_oid);

CREATE TABLE ValintakoeOsallistuminen (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    haku_oid varchar(127) NOT NULL,
    hakemus_oid varchar(127) UNIQUE,
    hakija_oid varchar(127) NOT NULL,
    created_at timestamp with time zone default now()
);

CREATE INDEX valintakoeosallistuminen_haku ON ValintakoeOsallistuminen(haku_oid);
CREATE INDEX valintakoeosallistuminen_hakija ON ValintakoeOsallistuminen(hakija_oid);
CREATE INDEX valintakoeosallistuminen_hakemus ON ValintakoeOsallistuminen(hakemus_oid);

CREATE TABLE Hakutoive (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    hakukohde_oid varchar(127) NOT NULL,
    laskettava_hakukohde_oid varchar(127) NOT NULL,
    valintakoe_osallistuminen uuid,
    CONSTRAINT fk_valintakoeosallistuminen
        FOREIGN KEY(valintakoe_osallistuminen)
            REFERENCES ValintakoeOsallistuminen(id)
            ON DELETE CASCADE
);

CREATE INDEX hakutoive_hakukohde ON Hakutoive(hakukohde_oid);
CREATE INDEX hakutoive_laskettavahakukohde ON Hakutoive(laskettava_hakukohde_oid);

CREATE TABLE ValintakoeValinnanVaihe (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    valinnanvaihe uuid NOT NULL,
    valinnan_vaihe_jarjestysluku int,
    laskettava_jarjestysluku int,
    hakutoive uuid not null,
    CONSTRAINT fk_hakutoive
        FOREIGN KEY(hakutoive)
            REFERENCES Hakutoive(id),
    CONSTRAINT fk_valinnanvaihe
        FOREIGN KEY(valinnanvaihe)
        REFERENCES Valinnanvaihe(id)
);

CREATE TABLE Valintakoe (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    valintakoe_oid varchar(127) NOT NULL,
    valintakoe_tunniste varchar(127),
    nimi varchar(255),
    aktiivinen boolean DEFAULT NULL,
    valintakoe_valinnanvaihe uuid,
    lahetaanko_koekutsut boolean DEFAULT NULL,
    kutsun_kohde varchar(255),
    kutsun_kohde_avain varchar(255),
    osallistuminen varchar(255),
    kuvaus_fi text,
    kuvaus_sv text,
    kuvaus_en text,
    laskenta_tila varchar(255),
    laskenta_tulos boolean,
    tekninen_kuvaus text,
    CONSTRAINT fk_valinnanvaihe
        FOREIGN KEY(valintakoe_valinnanvaihe)
            REFERENCES ValintakoeValinnanVaihe(id)
);

CREATE TABLE Valintatapajono (
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
    valinnanvaihe_id uuid NOT NULL,
    sijoitteluajo_id int,
    poissa_oleva_taytto boolean DEFAULT NULL,
    CONSTRAINT fk_valinnanvaihe
        FOREIGN KEY(valinnanvaihe_id)
            REFERENCES Valinnanvaihe(id)
);

CREATE TABLE Hakijaryhma (
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

CREATE INDEX hakijaryhma_hakukohde ON Hakijaryhma(hakukohde_oid);

CREATE TABLE Jonosija (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at timestamp with time zone default now(),
    etunimi varchar(255),
    sukunimi varchar(255),
    hakemus_oid varchar(127),
    hakija_oid varchar(127),
    hakutoiveprioriteetti int,
    harkinnanvarainen boolean DEFAULT NULL,
    hylatty_valisijoittelussa boolean DEFAULT NULL,
    valintatapajono_id uuid,
    hakijaryhma_id uuid,
    funktio_tulokset jsonb,
    syotetyt_arvot jsonb,
    CONSTRAINT fk_valintatapajono
        FOREIGN KEY(valintatapajono_id)
            REFERENCES Valintatapajono(id),
    CONSTRAINT fk_hakijaryhma
        FOREIGN KEY(hakijaryhma_id)
            REFERENCES Hakijaryhma(id)
);

CREATE INDEX jonosija_hakemus ON Jonosija(hakemus_oid);

CREATE TABLE MuokattuJonosija (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at timestamp with time zone default now(),
    hakukohde_oid varchar(127) NOT NULL,
    haku_oid varchar(127) NOT NULL,
    valintatapajono uuid NOT NULL,
    hakemus_oid varchar(127) NOT NULL,
    selite varchar(255),
    muokkaaja varchar(127) NOT NULL,
    muutos varchar(255),
    CONSTRAINT fk_valintatapajono
      FOREIGN KEY(valintatapajono)
          REFERENCES Valintatapajono(id)
);

CREATE INDEX muokattujonosija_hakukohde ON MuokattuJonosija(hakukohde_oid);
CREATE INDEX muokattujonosija_haku ON MuokattuJonosija(haku_oid);

CREATE TABLE Jarjestyskriteeritulos (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at timestamp with time zone default now(),
    prioriteetti int,
    arvo varchar(255),
    tila varchar(100),
    nimi varchar(255),
    kuvaus_fi text NOT NULL DEFAULT '',
    kuvaus_sv text NOT NULL DEFAULT '',
    kuvaus_en text NOT NULL DEFAULT '',
    tekninenKuvaus text,
    jonosija uuid,
    muokattu_jonosija uuid,
    CONSTRAINT fk_jonosija
        FOREIGN KEY(jonosija)
            REFERENCES Jonosija(id),
    CONSTRAINT fk_muokattujonosija
        FOREIGN KEY(muokattu_jonosija)
            REFERENCES MuokattuJonosija(id)
);




