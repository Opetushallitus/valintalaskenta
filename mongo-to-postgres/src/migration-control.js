import {getDistinctHakukohteetAndHaut} from "./get-from-mongo.js";

const AMOUNT_TO_FETCH_AT_ONCE = 100;
const CONTROL_TABLE = 'data_migration_control';
const BEFORE_DATE = new Date(Date.UTC(2022, 0, 1));
const AFTER_DATE = new Date(Date.UTC(2021, 11, 31));

const populateControlTable = async (knex, mongoConn, useAfter = false) => {
  let result;
  if (useAfter) {
    console.log('Fetching hakukohde and haku data newer than ' + AFTER_DATE);
    result = await getDistinctHakukohteetAndHaut(mongoConn, null, AFTER_DATE);
  } else {
    console.log('Fetching hakukohde and haku data older than ' + BEFORE_DATE);
    result = await getDistinctHakukohteetAndHaut(mongoConn, BEFORE_DATE);
  }

  await knex.transaction(async trx => {
    for (const obj of result) {
      await trx(CONTROL_TABLE)
        .insert({hakukohde_oid: obj.hakukohde, haku_oid: obj.haku, created_at: obj.createdAt});
    }
  });

}

export const fetchFromMigrationControl = async (knex, mongoConn, useAfter = false) => {
  
  let populated;
  
  if (useAfter) {
    populated = await knex(CONTROL_TABLE)
      .select('id')
      .where('created_at', '>', AFTER_DATE)
      .limit(1);
  } else {
    populated = await knex(CONTROL_TABLE)
      .select('id')
      .where('created_at', '<', BEFORE_DATE)
      .limit(1);
  }

  console.log(populated);

  if (!populated || populated.length < 1) {
    await populateControlTable(knex, mongoConn, useAfter);
  }
  
  const data = await knex(CONTROL_TABLE)
    .select('haku_oid', 'hakukohde_oid')
    .whereNull('success')
    .orderBy('id', 'asc')
    .limit(AMOUNT_TO_FETCH_AT_ONCE);

  return data.map(d => {
    return {haku: d.haku_oid, hakukohde: d.hakukohde_oid};
  });
}

export const updateMigrationRow = async (knex, oidObj, isSuccess, error, totalSeconds) => {
  const isHaku = oidObj.hakukohde == null;
  const oid = isHaku? oidObj.haku : oidObj.hakukohde;
  console.log(`updateMigrationRow, ${oid}, ${isHaku}, ${isSuccess}`);
  if (isHaku) {
    await knex(CONTROL_TABLE)
    .where('haku_oid', '=', oid)
    .whereNull('hakukohde_oid')
    .update({ updated_at: new Date().toUTCString(),
              success: isSuccess,
              error_message: error,
              duration_in_seconds: totalSeconds});
  } else {
    await knex(CONTROL_TABLE)
    .where('hakukohde_oid', '=', oid)
    .update({ updated_at: new Date().toUTCString(),
              success: isSuccess,
              error_message: error,
              duration_in_seconds: totalSeconds});
  }

}

