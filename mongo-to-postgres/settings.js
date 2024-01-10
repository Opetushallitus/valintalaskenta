import {mongoPass, possuPass} from './pass.js';

const connections = {
  mongo: `mongodb://valintalaskentauser:${mongoPass}@localhost:57117/valintalaskentadb`,
  postgres: `postgres://oph:${possuPass}h@localhost:5432/valintalaskenta`
};

const formJonosijaCollection = (foreignKey, parentField) => {
  return   {
    collectionName: 'Jonosija',
    tableName: 'jonosija',
    fieldsToCopy: [
      ['hakemusOid', 'hakemus_oid'],
      ['hakijaOid', 'hakija_oid'],
      ['hakutoiveprioriteetti', 'hakutoiveprioriteetti'],
      ['harkinnanvarainen', 'harkinnanvarainen'],
      ['hylattyValisijoittelussa', 'hylatty_valisijoittelussa'],
      ['funktioTulokset', 'funktio_tulokset'],
      ['syotetytArvot', 'syotetyt_arvot'],
      ['jarjestyskriteeritulokset', 'jarjestyskriteeritulokset']
    ],
    foreignKey,
    parentField,
    jsonFields: ['syotetytArvot', 'funktioTulokset', 'jarjestyskriteeritulokset']
  }
}

//TODO: remember to add column muokkaaja to muokattu_jonosija table
const getValuesFromLatestLogEntry = (row) => {
  const logEntries = row.logEntries;
  if (logEntries.length < 1) {
    return {};
  }
  const latestLogEntry = logEntries[logEntries.length - 1];
  return {selite: latestLogEntry.selite, muutos: latestLogEntry.muutos, muokkaaja: latestLogEntry.muokkaaja};
};

const getValuesFromOsallistuminenTulos = (row) => {
  const osallistuminenTulos = row.osallistuminenTulos;
  const kuvausFi = osallistuminenTulos?.kuvaus?.FI || '';
  const kuvausEn = osallistuminenTulos?.kuvaus?.EN || '';
  const kuvausSv = osallistuminenTulos?.kuvaus?.SV || '';
  return {
    osallistuminen: osallistuminenTulos.osallistuminen,
    kuvaus_fi: kuvausFi,
    kuvaus_en: kuvausEn,
    kuvaus_sv: kuvausSv,
    laskenta_tila: osallistuminenTulos.laskentaTila,
    laskenta_tulos: osallistuminenTulos.laskentaTulos,
    tekninen_kuvaus: osallistuminenTulos.tekninenKuvaus
  }
}

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
    ],
    subCollection: {
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
        ['sijoitteluajoId', 'sijoitteluajo_id']
      ],
      parentField: 'valintatapajonot',
      foreignKey: 'valinnanvaihe',
      ordered: 'valinnanvaihe_key',
      subCollection: formJonosijaCollection('valintatapajono', 'jonosijaIdt')
    }
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
    ],
    subCollection: formJonosijaCollection('hakijaryhma', 'jonosijaIdt')
  },
  {
    collectionName: 'MuokattuJonosija',
    tableName: 'muokattu_jonosija',
    fieldsToCopy: [
      ['hakukohdeOid', 'hakukohde_oid'],
      ['hakuOid', 'haku_oid'],
      ['valintatapajonoOid', 'valintatapajono_oid'],
      ['hakemusOid', 'hakemus_oid'],
      ['jarjestyskriteerit', 'jarjestyskriteeritulokset'],
    ],
    jsonFields: ['jarjestyskriteerit'],
    getMoreFieldsToAddFn: getValuesFromLatestLogEntry
  },
  {
    collectionName: 'ValintakoeOsallistuminen',
    tableName: 'valintakoe_osallistuminen',
    useHaku: true,
    fieldsToCopy: [
      ['hakuOid', 'haku_oid'],
      ['hakemusOid', 'hakemus_oid'],
      ['hakijaOid', 'hakija_oid'],
      ['createdAt', 'created_at']
    ],
    subCollection: {
      tableName: 'hakutoive',
      fieldsToCopy: [
        ['hakukohdeOid', 'hakukohde_oid'],
      ],
      parentField: 'hakutoiveet',
      foreignKey: 'valintakoe_osallistuminen',
      embbeddedCollection: true,
      subCollection: {
        embbeddedCollection: true,
        tableName: 'valintakoe_valinnanvaihe',
        fieldsToCopy: [
          ['valinnanVaiheOid', 'valinnanvaihe_oid'],
          ['valinnanVaiheJarjestysluku', 'valinnan_vaihe_jarjestysluku'],
        ],
        parentField: 'valinnanVaiheet',
        foreignKey: 'hakutoive',
        subCollection: {
          embbeddedCollection: true,
          tableName: 'valintakoe',
          fieldsToCopy: [
            ['valintakoeOid', 'valintakoe_oid'],
            ['valintakoeTunniste', 'valintakoe_tunniste'],
            ['nimi', 'nimi'],
            ['aktiivinen', 'aktiivinen'],
            ['lahetetaankoKoekutsut', 'lahetetaanko_koekutsut'],
            ['kutsuttavienMaara', 'kutsuttavien_maara'],
            ['kutsunKohde', 'kutsun_kohde'],
            ['kutsunKohdeAvain', 'kutsun_kohde_avain'],
          ],
          parentField: 'valintakokeet',
          foreignKey: 'valintakoe_valinnanvaihe',
          getMoreFieldsToAddFn: getValuesFromOsallistuminenTulos
        }
      },
    },
  }
];

export { connections, collections };