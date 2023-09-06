CREATE TABLE Hakijaryhma (
    id uuid PRIMARY KEY,
    hakijaryhmaOid varchar(127) UNIQUE,
    prioriteetti int,
    createdAt timestamp with time zone default now(),
    hakukohdeOid varchar(127) NOT NULL,
    nimi varchar(255),
    kiintio int,
    kaytaKaikki boolean DEFAULT NULL,
    tarkkaKiintio boolean DEFAULT NULL,
    kaytetaanRyhmaanKuuluvia boolean DEFAULT NULL,
    hakijaryhmatyyppiKoodiuri varchar(255)
);

CREATE INDEX hakijaryhma_hakukohde ON Hakijaryhma(hakukohdeOid);

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
    createdAt timestamp with time zone default now(),
    hakutoiveet jsonb
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

CREATE TABLE HakijanValinnanVaihe (
    id uuid PRIMARY KEY,
    valinnanVaiheOid varchar(127) NOT NULL,
    valinnanVaiheJarjestysluku int,
    laskettavaJarjestysluku int,
    hakutoive uuid,
    CONSTRAINT fk_hakutoive
        FOREIGN KEY(hakutoive)
            REFERENCES Hakutoive(id)
            ON DELETE CASCADE,
    CONSTRAINT fk_valinnanvaihe
        FOREIGN KEY(valinnanVaiheOid)
        REFERENCES Valinnanvaihe(valinnanvaiheOid)
        ON DELETE CASCADE
);

CREATE TABLE Hakijankoe (
    id uuid PRIMARY KEY,
    valintakoeOid varchar(127) NOT NULL,
    valintakoeTunniste varchar(127),
    nimi varchar(255),
    aktiivinen boolean DEFAULT NULL,
    valinnanvaihe uuid,
    lahetaankoKoekutsut boolean DEFAULT NULL,
    kutsunKohde varchar(255),
    CONSTRAINT fk_valinnanvaihe
        FOREIGN KEY(valinnanvaihe)
            REFERENCES Valinnanvaihe(id)
            ON DELETE CASCADE
);

CREATE TABLE OsallistuminenTulos (
    osallistuminen varchar(255),
    kuvausFI text,
    kuvausSV text,
    kuvausEN text,
    laskentatila varchar(255),
    valintakoe uuid,
    CONSTRAINT fk_valintakoe
        FOREIGN KEY(valintakoe)
            REFERENCES Hakijankoe(id)
            ON DELETE CASCADE
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
    valinnanvaiheOid varchar(127) NOT NULL,
    CONSTRAINT fk_valinnanvaihe
        FOREIGN KEY(valinnanvaiheOid)
            REFERENCES Valinnanvaihe(valinnanvaiheOid)
);

CREATE TABLE Jonosija (
    id uuid PRIMARY KEY,
    createdAt timestamp with time zone default now(),
    hakemusOid varchar(127) NOT NULL,
    hakijaOid varchar(127) NOT NULL,
    hakutoiveprioriteetti int,
    harkinnanvarainen boolean DEFAULT NULL,
    hylattyValisijoittelussa boolean DEFAULT NULL,
    valintatapajonoOid varchar(127) NOT NULL,
    hakijaryhmaOid varchar(127) NOT NULL,
    funktioTulokset jsonb,
    syotetytArvot jsonb,
    CONSTRAINT fk_valintatapajono
        FOREIGN KEY(valintatapajonoOid)
            REFERENCES Valintatapajono(valintatapajonoOid),
    CONSTRAINT fk_hakijaryhma
        FOREIGN KEY(hakijaryhmaOid)
            REFERENCES Hakijaryhma(hakijaryhmaOid)
);

CREATE INDEX jonosija_hakemus ON Jonosija(hakemusOid);

CREATE TABLE MuokattuJonosija (
    id uuid PRIMARY KEY,
    createdAt timestamp with time zone default now(),
    hakukohdeOid varchar(127) NOT NULL,
    hakuOid varchar(127) NOT NULL,
    valintatapajonoOid varchar(127) NOT NULL,
    hakemusOid varchar(127) NOT NULL,
    selite varchar(255),
    muokkaaja varchar(127) NOT NULL,
    muutos varchar(255),
    CONSTRAINT fk_valintatapajono
      FOREIGN KEY(valintatapajonoOid)
          REFERENCES Valintatapajono(valintatapajonoOid)
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
    kuvausFI text,
    kuvausSV text,
    kuvausEN text,
    tekninenKuvaus text,
    jonosija uuid DEFAULT NULL,
    muokattujonosija uuid DEFAULT NULL,
    CONSTRAINT fk_jonosija
        FOREIGN KEY(jonosija)
            REFERENCES Jonosija(id),
    CONSTRAINT fk_muokattujonosija
        FOREIGN KEY(muokattujonosija)
            REFERENCES MuokattuJonosija(id)
);




