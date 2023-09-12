CREATE TABLE HarkinnanvarainenHyvaksyminen (
    id uuid PRIMARY KEY,
    createdAt timestamp with time zone default now(),
    harkinnanvaraisuustila varchar(255),
    hakukohdeOid varchar(127) NOT NULL,
    hakemusOid varchar(127) NOT NULL,
    hakuOid varchar(127) NOT NULL
);

CREATE TABLE Valinnanvaihe (
    id uuid PRIMARY KEY,
    jarjestysnumero int,
    createdAt timestamp with time zone default now(),
    hakuOid varchar(127) NOT NULL,
    hakukohdeOid varchar(127) NOT NULL,
    valinnanvaiheOid varchar(127) UNIQUE NOT NULL,
    tarjoajaOid varchar(127) NOT NULL,
    nimi varchar(255)
);

CREATE INDEX valinnanvaihe_hakukohde ON Valinnanvaihe(hakukohdeOid);
CREATE INDEX valinnanvaihe_haku ON Valinnanvaihe(hakuOid);

CREATE TABLE ValintakoeOsallistuminen (
    id uuid PRIMARY KEY,
    hakuOid varchar(127) NOT NULL,
    hakemusOid varchar(127) UNIQUE,
    hakijaOid varchar(127) NOT NULL,
    createdAt timestamp with time zone default now()
);

CREATE INDEX valintakoeosallistuminen_haku ON ValintakoeOsallistuminen(hakuOid);
CREATE INDEX valintakoeosallistuminen_hakija ON ValintakoeOsallistuminen(hakijaOid);
CREATE INDEX valintakoeosallistuminen_hakemus ON ValintakoeOsallistuminen(hakemusOid);

CREATE TABLE Hakutoive (
    id uuid PRIMARY KEY,
    hakukohdeOid varchar(127) NOT NULL,
    laskettavaHakukohdeOid varchar(127) NOT NULL,
    valintakoeOsallistuminen uuid,
    CONSTRAINT fk_valintakoeosallistuminen
        FOREIGN KEY(valintakoeOsallistuminen)
            REFERENCES ValintakoeOsallistuminen(id)
            ON DELETE CASCADE
);

CREATE INDEX hakutoive_hakukohde ON Hakutoive(hakukohdeOid);
CREATE INDEX hakutoive_laskettavahakukohde ON Hakutoive(laskettavahakukohdeOid);

CREATE TABLE ValintakoeValinnanVaihe (
    id uuid PRIMARY KEY,
    valinnanvaihe uuid NOT NULL,
    valinnanVaiheJarjestysluku int,
    laskettavaJarjestysluku int,
    hakutoive uuid not null,
    CONSTRAINT fk_hakutoive
        FOREIGN KEY(hakutoive)
            REFERENCES Hakutoive(id),
    CONSTRAINT fk_valinnanvaihe
        FOREIGN KEY(valinnanvaihe)
        REFERENCES Valinnanvaihe(id)
);

CREATE TABLE Valintakoe (
    id uuid PRIMARY KEY,
    valintakoeOid varchar(127) NOT NULL,
    valintakoeTunniste varchar(127),
    nimi varchar(255),
    aktiivinen boolean DEFAULT NULL,
    valintakoeValinnanvaihe uuid,
    lahetaankoKoekutsut boolean DEFAULT NULL,
    kutsunKohde varchar(255),
    kutsunKohdeAvain varchar(255),
    osallistuminen varchar(255),
    kuvausFI text,
    kuvausSV text,
    kuvausEN text,
    laskentatila varchar(255),
    laskentaTulos boolean,
    tekninenKuvaus text,
    CONSTRAINT fk_valinnanvaihe
        FOREIGN KEY(valintakoeValinnanvaihe)
            REFERENCES ValintakoeValinnanVaihe(id)
);

CREATE TABLE Valintatapajono (
    id uuid PRIMARY KEY,
    createdAt timestamp with time zone default now(),
    valintatapajonoOid varchar(127) UNIQUE,
    nimi varchar(255),
    prioriteetti int,
    aloituspaikat int,
    siirretaanSijoitteluun boolean DEFAULT NULL,
    tasasijasaanto varchar(100),
    eiVarasijatayttoa boolean DEFAULT NULL,
    kaikkiEhdonTayttavatHyvaksytaan boolean DEFAULT NULL,
    kaytetaanValintalaskentaa boolean DEFAULT NULL,
    valmisSijoiteltavaksi boolean DEFAULT NULL,
    kaytetaanKokonaispisteita boolean DEFAULT NULL,
    valinnanvaihe uuid NOT NULL,
    sijoitteluajoId int,
    CONSTRAINT fk_valinnanvaihe
        FOREIGN KEY(valinnanvaihe)
            REFERENCES Valinnanvaihe(id)
);

CREATE TABLE Hakijaryhma (
     id uuid PRIMARY KEY,
     hakijaryhmaOid varchar(127) UNIQUE,
     prioriteetti int,
     createdAt timestamp with time zone default now(),
     hakukohdeOid varchar(127) NOT NULL,
     nimi varchar(255),
     kuvaus text,
     kiintio int,
     kaytaKaikki boolean DEFAULT NULL,
     tarkkaKiintio boolean DEFAULT NULL,
     kaytetaanRyhmaanKuuluvia boolean DEFAULT NULL,
     hakijaryhmatyyppiKoodiuri varchar(255),
     valintatapajono uuid DEFAULT NULL,
     CONSTRAINT fk_valintatapajono
        FOREIGN KEY (valintatapajono)
        REFERENCES Valintatapajono(id)
);

CREATE INDEX hakijaryhma_hakukohde ON Hakijaryhma(hakukohdeOid);

CREATE TABLE Jonosija (
    id uuid PRIMARY KEY,
    createdAt timestamp with time zone default now(),
    hakemusOid varchar(127) NOT NULL,
    hakijaOid varchar(127) NOT NULL,
    hakutoiveprioriteetti int,
    harkinnanvarainen boolean DEFAULT NULL,
    hylattyValisijoittelussa boolean DEFAULT NULL,
    valintatapajono uuid NOT NULL,
    hakijaryhma uuid NOT NULL,
    funktioTulokset jsonb,
    syotetytArvot jsonb,
    CONSTRAINT fk_valintatapajono
        FOREIGN KEY(valintatapajono)
            REFERENCES Valintatapajono(id),
    CONSTRAINT fk_hakijaryhma
        FOREIGN KEY(hakijaryhma)
            REFERENCES Hakijaryhma(id)
);

CREATE INDEX jonosija_hakemus ON Jonosija(hakemusOid);

CREATE TABLE MuokattuJonosija (
    id uuid PRIMARY KEY,
    createdAt timestamp with time zone default now(),
    hakukohdeOid varchar(127) NOT NULL,
    hakuOid varchar(127) NOT NULL,
    valintatapajono uuid NOT NULL,
    hakemusOid varchar(127) NOT NULL,
    selite varchar(255),
    muokkaaja varchar(127) NOT NULL,
    muutos varchar(255),
    CONSTRAINT fk_valintatapajono
      FOREIGN KEY(valintatapajono)
          REFERENCES Valintatapajono(id)
);

CREATE INDEX muokattujonosija_hakukohde ON MuokattuJonosija(hakukohdeOid);
CREATE INDEX muokattujonosija_haku ON MuokattuJonosija(hakuOid);

CREATE TABLE Jarjestyskriteeritulos (
    id uuid PRIMARY KEY,
    createdAt timestamp with time zone default now(),
    prioriteetti int,
    arvo varchar(255),
    tila varchar(100),
    nimi varchar(255),
    kuvausFI text NOT NULL DEFAULT '',
    kuvausSV text NOT NULL DEFAULT '',
    kuvausEN text NOT NULL DEFAULT ä,
    tekninenKuvaus text,
    jonosija uuid DEFAULT NULL,
    muokattuJonosija uuid DEFAULT NULL,
    CONSTRAINT fk_jonosija
        FOREIGN KEY(jonosija)
            REFERENCES Jonosija(id),
    CONSTRAINT fk_muokattujonosija
        FOREIGN KEY(muokattuJonosija)
            REFERENCES MuokattuJonosija(id)
);




