const mongoPass = "";
const possuPass = "";

const connections = {
  mongo: `mongodb://valintalaskentauser:${mongoPass}@localhost:57117/valintalaskentadb`,
  postgres: `postgres://oph:${possuPass}h@localhost:5432/valintalaskenta`
};

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

  }
];

export { connections, collections };