import {mongoPass, possuPass} from './pass.js';

const connections = {
  mongo: `mongodb://valintalaskentauser:${mongoPass}@localhost:57117/valintalaskentadb`,
  postgres: `postgres://oph:${possuPass}h@localhost:5432/valintalaskenta`
};

//TODO: jarjestyskriteerihistoria needs extra care among others

const collections = [
  {
    collectionName: 'HarkinnanvarainenHyvaksyminen',
    tableName: 'harkinnanvarainen_hyvaksyminen',
    fieldsToCopy: [
      ['harkinnanvaraisuusTila', 'harkinnanvaraisuus_tila'],
      ['hakukohdeOid', 'hakukohde_oid'],
      ['hakemusOid', 'hakemus_oid'],
      ['hakuOid', 'haku_oid']
    ]
  },
  {
    collectionName: 'Valinnanvaihe',
    tableName: 'valinnanvaihe',
    fieldsToCopy: [
      ['jarjestysnumero', 'jarjestysnumero'],
      ['createdAt', 'created_at'],
      ['hakuOid', 'haku_oid'],
      ['hakukohdeOid', 'hakukohde_oid'],
      ['valinnanvaiheOid', 'valinnanvaihe_oid'],
      ['tarjoajaOid', 'tarjoaja_oid'],
      ['nimi', 'nimi'],
    ]
  },
  {
    collectionName: 'Valintatapajono',
    tableName: 'valintatapajono',
    fieldsToCopy: [
      ['valintatapajonoOid', 'valintatapajono_oid'],
      ['nimi', 'nimi'],
      ['prioriteetti', 'prioriteetti'],
      ['aloituspaikat', 'aloituspaikat'],
      ['siirretaanSijoitteluun', 'siirretaan_sijoitteluun'],
      ['tasasijasaanto', 'tasasijasaanto'],
      ['eiVarasijatayttoa', 'ei_varasijatayttoa'],
      ['kaikkiEhdonTayttavatHyvaksytaan', 'kaikki_ehdon_tayttavat_hyvaksytaan'],
      ['kaytetaanValintalaskentaa', 'kaytetaan_valintalaskentaa'],
      ['poissaOlevaTaytto', 'poissa_oleva_taytto'],
      ['valmisSijoiteltavaksi', 'valmis_sijoiteltavaksi'],
      ['kaytetaanKokonaispisteita', 'kaytetaan_kokonaispisteita'],
      //TODO reference to valinnanvaihe
      ['sijoitteluajoId', 'sijoitteluajo_id']
    ]
  },
  {
    collectionName: 'Hakijaryhma',
    tableName: 'hakijaryhma',
    fieldsToCopy: [
      ['hakijaryhmaOid', 'hakijaryhma_oid'],
      ['prioriteetti', 'prioriteetti'],
      ['createdAt', 'created_at'],
      ['hakukohdeOid', 'hakukohde_oid'],
      ['nimi', 'nimi'],
      ['kuvaus', 'kuvaus'],
      ['kiintio', 'kiintio'],
      ['kaytaKaikki', 'kayta_kaikki'],
      ['tarkkaKiintio', 'tarkka_kiintio'],
      ['kaytetaanRyhmaanKuuluvia', 'kaytetaan_ryhmaan_kuuluvia'],
      ['hakijaryhmatyyppikoodiUri', 'hakijaryhmatyyppi_koodiuri'],
      ['valintatapajonoOid', 'valintatapajono_oid']
    ]
  },
  {
    collectionName: 'Jonosija',
    tableName: 'jonosija',
    fieldsToCopy: [
      ['hakemusOid', 'hakemus_oid'],
      ['hakijaOid', 'hakija_oid'],
      ['hakutoiveprioriteetti', 'hakutoiveprioriteetti'],
      ['harkinnanvarainen', 'harkinnanvarainen'],
      ['hylattyValisijoittelussa', 'hylatty_valisijoittelussa'],
      //TODO: valintatapajono, relaatio possuksi
      //TODO: hakijaryhma, relaatio possuksi
      ['funktioTulokset', 'funktio_tulokset'],
      ['syotetytArvot', 'syotetyt_arvot'],
      ['jarjestyskriteeritulokset', 'jarjestyskriteeritulokset']
    ],
  },
  {
    collectionName: 'MuokattuJonosija',
    tableName: 'muokattu_jonosija',
    fieldsToCopy: [
      ['hakukohdeOid', 'hakukohde_oid'],
      ['hakuOid', 'haku_oid'],
      ['valintatapajonoOid', 'valintatapajono_oid'],
      ['hakemusOid', 'hakemus_oid']
      ['jarjestyskriteerit', 'jarjestyskriteeritulokset']
      //TODO include fields from LogEntry
    ]
  },
  //TODO maybe handle following entities separately? hakuOid can be entry point
  {
    collectionName: 'ValintakoeOsallistuminen',
    tableName: 'valintakoe_osallistuminen',
    fieldsToCopy: [
      ['hakuOid', 'haku_oid'],
      ['hakemusOid', 'hakemus_oid'],
      ['hakijaOid', 'hakija_oid'],
      ['createdAt', 'created_at']
    ]
  },
  {
    collectionName: 'Hakutoive',
    tableName: 'hakutoive',
    fieldsToCopy: [
      ['hakukohdeOid', 'hakukohde_oid'],
      //TODO reference to valintakoe_osallistuminen
    ]
  },
  {
    collectionName: 'Valintakoe',
    tableName: 'valintakoe',
    fieldsToCopy: [
      ['valintakoeOid', 'valintakoe_oid'],
      ['valintakoeTunniste', 'valintakoe_tunniste'],
      ['nimi', 'nimi'],
      ['aktiivinen', 'aktiivinen'],
      ['lahetetaankoKoekutsut', 'lahetetaanko_koekutsut'],
      ['kutsuttavienMaara', 'kutsuttavienMaara'],
      ['kutsunKohde', 'kutsun_kohde'],
      ['kutsunKohdeAvain', 'kutsun_kohde_avain'],
      //TODO rest of the fields from collection OsallistuminenTulos
    ]
  }

];

export { connections, collections };