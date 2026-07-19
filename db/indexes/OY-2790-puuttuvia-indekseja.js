db.Valinnanvaihe.createIndex(
  {
      "hakuOid": 1,
      "valintatapajonot.$id": 1
  },
  {
      name: "idx_hakuoid_valintatapajonot_id",
      unique: false,
      background: true
  }
);

db.Valintatapajono.createIndex(
  {
      "jonosijaIdt": 1
  },
  {
      name: "idx_jonosija_idt",
      unique: false,
      background: true
  }
);

db.HarkinnanvarainenHyvaksyminen.createIndex(
  {
      "hakuOid": 1,
      "hakemusOid": 1
  },
  {
      name: "idx_hakuoid_hakemusoid",
      unique: false,
      background: true
  }
);

db.HarkinnanvarainenHyvaksyminen.createIndex(
  {
      "hakukohdeOid": 1
  },
  {
      name: "idx_hakukohdeoid",
      unique: false,
      background: true
  }
);
