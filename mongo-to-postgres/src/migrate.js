import mongoose from 'mongoose';
import Knex from 'knex';
import getFromMongo, {fillMongoObject} from './get-from-mongo.js';
import putToPostgres from './put-to-postgres.js';
import {fetchFromMigrationControl, updateMigrationRow} from './migration-control.js';

export default async ({ connections, collections, collectionsForHaku }) => {
  console.log('Starting migration...');

  let mongooseConn;
  process.stdout.write('Connection to mongo... ');
  try {
    mongooseConn = await mongoose.connect(connections.mongo,
      {
        readPreference: 'secondary',
        authMechanism: 'SCRAM-SHA-1',
        connectTimeoutMS: 5000,
        authSource: 'admin',
        directConnection: true,
      });
    console.log(mongooseConn.readyState);  
    console.log('connected.');
  } catch (err) {
    console.log('ERROR MONGO');
    console.log(err);
  }

  let knex;
  process.stdout.write('Connection to postgres... ');
  try {
    knex = Knex({
      client: 'pg',
      connection: connections.postgres
    });
    console.log('connected.');
  } catch (err) {
    console.log('ERROR POSTGRES');
    console.err(err);
  }

  const oidsToProcess = await fetchFromMigrationControl(knex, mongooseConn);
  console.log(oidsToProcess);

  const timeRoundStarted = Date.now();

  performProcess(mongooseConn, knex, collections, collectionsForHaku, oidsToProcess)
    .then(() => console.log(`\n\nFinished successfully. Total duration in seconds ${Math.round((Date.now() - timeRoundStarted) / 1000)}`))
    .catch((err) => console.error(err))
    .finally(() => process.exit(0));
};


async function performProcess(mongooseConn, knex, collections, collectionsForHaku, oidsToProcess) {
  for (const oid of oidsToProcess) {
    try {
      let totalSeconds = 0;
      await knex.transaction(async trx => {
        if (oid.hakukohde) {
          totalSeconds = await copyHakukohdeData(mongooseConn, trx, collections, oid.hakukohde);
        } else {
          totalSeconds = await copyHakuData(mongooseConn, trx, collectionsForHaku, oid.haku);
        }
      });
      await updateMigrationRow(knex, oid, true, null, totalSeconds);
    } catch (error) {
      await updateMigrationRow(knex, oid, false, error, null);
      console.error(error);
    }
  }
}

async function copyHakuData(mongooseConn, trx, collections, hakuOid) {
  const timeStarted = Date.now();
  console.log(`\nMigrating haku ${hakuOid}`);
  for (const collection of collections) {
    const rows = await getFromMongo(mongooseConn, collection.collectionName, hakuOid, true);
    const filledRows = await fillMongoObject({collections, tableName: collection.tableName, rows, mongooseConn});
    console.log(`Fetching hakukohde data took ${Math.round((Date.now() - timeStarted) / 1000)} for hakukohde ${hakukohdeOid}`);
    await putToPostgres({
      trx,
      collections,
      tableName: collection.tableName,
      rows: filledRows
    });
  }
  const totalSeconds = Math.round((Date.now() - timeStarted) / 1000);
  console.log(`Took ${totalSeconds} seconds to copy haku data for ${hakuOid}`);
  return totalSeconds;
}

async function copyHakukohdeData(mongooseConn, trx, collections, hakukohdeOid) {
  const timeStarted = Date.now();
  console.log(`\nMigrating hakukohde ${hakukohdeOid}`);
  for (const collection of collections) {
    const rows = await getFromMongo(mongooseConn, collection.collectionName, hakukohdeOid);
    const filledRows = await fillMongoObject({collections, tableName: collection.tableName, rows, mongooseConn});
    if (collection.collectionName == 'Valinnanvaihe') {
      console.log(`Fetching hakukohde data took ${Math.round((Date.now() - timeStarted) / 1000)} for hakukohde ${hakukohdeOid}`);
    }
    await putToPostgres({
      trx,
      collections,
      tableName: collection.tableName,
      rows: filledRows
    });
  }
  const totalSeconds = Math.round((Date.now() - timeStarted) / 1000);
  console.log(`Took ${totalSeconds} seconds to copy hakukohde data for ${hakukohdeOid}`);
  return totalSeconds;
}


