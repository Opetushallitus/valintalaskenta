CREATE TABLE Hakijaryhma (
    id uuid PRIMARY KEY,
    hakijaryhmaOid varchar(127) UNIQUE,
    prioriteetti int,
    createdAt timestamp with time zone default now(),
    hakukohdeOid varchar(127),
    nimi varchar(255),
    kiintio int,
    kaytaKaikki boolean,
    tarkkaKiintio boolean,
    kaytetaanRyhmaanKuuluvia boolean,
    hakijaryhmatyyppiKoodiuri varchar(255)
);

CREATE INDEX hakijaryhma_hakukohde ON Hakijaryhma(hakukohdeOid);

CREATE TABLE HarkinnanvarainenHyvaksyminen (
    id uuid PRIMARY KEY,
    harkinnanvaraisuustila varchar(255),
    hakukohdeOid varchar(127),
    hakemusOid varchar(127),
    hakuOid varchar(127)
);

CREATE TABLE Valinnanvaihe (
    id uuid PRIMARY KEY,
    jarjestysnumero int,
    createdAt timestamp with time zone default now(),
    hakuOid varchar(127),
    hakukohdeOid varchar(127),
    valinnanvaiheOid varchar(127) UNIQUE,
    tarjoajaOid varchar(127),
    nimi varchar(255)
);

CREATE INDEX valinnanvaihe_hakukohde ON Valinnanvaihe(hakukohdeOid);
CREATE INDEX valinnanvaihe_haku ON Valinnanvaihe(hakuOid);

CREATE TABLE ValintakoeOsallistuminen (
    id uuid PRIMARY KEY,
    hakuOid varchar(127),
    hakemusOid varchar(127) UNIQUE,
    hakijaOid varchar(127),
    createdAt timestamp with time zone default now(),
    hakutoiveet jsonb,
);

CREATE INDEX valintakoeosallistuminen_haku ON ValintakoeOsallistuminen(hakuOid);
CREATE INDEX valintakoeosallistuminen_hakija ON ValintakoeOsallistuminen(hakijaOid);

CREATE TABLE Valintatapajono (
    id uuid PRIMARY KEY,
    valintatapajonoOid varchar(127) UNIQUE,
    nimi varchar(255),
    prioriteetti int,
    aloituspaikat int,
    siirretaanSijoitteluun boolean,
    tasasijasaanto varchar(100),
    eiVarasijatayttoa boolean,
    kaikkiEhdonTayttavatHyvaksytaan boolean,
    kaytetaanValintalaskentaa boolean,
    valmisSijoiteltavaksi boolean,
    valinnanvaiheOid varchar(127),
    CONSTRAINT fk_valinnanvaihe
        FOREIGN KEY(valinnanvaiheOid)
            REFERENCES Valinnanvaihe(valinnanvaiheOid)
)

CREATE TABLE Jonosija (
    id uuid PRIMARY KEY,
    hakemusOid varchar(127),
    hakijaOid varchar(127),
    hakutoiveprioriteetti int,
    harkinnanvarainen boolean,
    hylattyValisijoittelussa boolean,
    valintatapajonoOid varchar(127),
    hakijaryhmaOid varchar(127),
    jarjestyskriteeritulokset jsonb,
    CONSTRAINT fk_valintatapajono
        FOREIGN KEY(valintatapajonoOid)
            REFERENCES Valintatapajono(valintatapajonoOid),
    CONSTRAINT fk_hakijaryhma
        FOREIGN KEY(hakijaryhmaOid)
            REFERENCES Hakijaryhma(hakijaryhmaOid),
)

CREATE INDEX jonosija_hakemus ON Jonosija(hakemusOid);

CREATE TABLE MuokattuJonosija (
    id uuid PRIMARY KEY,
    hakukohdeOid varchar(127),
    hakuOid varchar(127),
    valintatapajonoOid varchar(127),
    hakemusOid varchar(127),
    jarjestyskriteeritulokset jsonb,
    logEntries jsonb,
    CONSTRAINT fk_valintatapajono
      FOREIGN KEY(valintatapajonoOid)
          REFERENCES Valintatapajono(valintatapajonoOid),
)

CREATE INDEX muokattujonosija_hakukohde ON MuokattuJonosija(hakukohdeOid);
CREATE INDEX muokattujonosija_haku ON MuokattuJonosija(hakuOid);
