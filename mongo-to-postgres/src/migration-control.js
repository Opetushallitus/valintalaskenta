import {getDistinctHakukohteetAndHaut} from "./get-from-mongo.js";

const AMOUNT_TO_FETCH_AT_ONCE = 3;

const populateControlTable = async (knex, mongoConn) => {
  const result = await getDistinctHakukohteetAndHaut(mongoConn);

  await knex.transaction(async trx => {
    for (const obj of result) {
      await trx('data_migration_control')
        .insert({hakukohde_oid : obj.hakukohde, haku_oid : obj.haku});
    }
  });

}

export const fetchFromMigrationControl = async (knex, mongoConn) => {
  
  const populated = await knex('data_migration_control')
    .select('id')
    .limit(1)

  console.log(populated);

  if (!populated || populated.length < 1) {
    await populateControlTable(knex, mongoConn);
  }
  
  const data = await knex('data_migration_control')
    .select('haku_oid', 'hakukohde_oid')
    .whereNull('success')
    .orderBy('id', 'asc')
    .limit(AMOUNT_TO_FETCH_AT_ONCE);

  return data.map(d => {
    return {haku: d.haku_oid, hakukohde: d.hakukohde_oid};
  });
}

