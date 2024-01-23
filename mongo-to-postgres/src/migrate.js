import mongoose from 'mongoose';
import Knex from 'knex';
import getFromMongo, {fillMongoObject, getFromMongoObjects} from './get-from-mongo.js';
import putToPostgres, {storeSubCollection} from './put-to-postgres.js';
import {fetchFromMigrationControl, updateMigrationRow} from './migration-control.js';

const SLEEP_TIME_SECONDS = 1;

function sleep(s) {
  return new Promise((resolve) => {
    setTimeout(resolve, s * 1000);
  });
}

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

  let oidsToProcess;

  do {
    oidsToProcess = await fetchFromMigrationControl(knex, mongooseConn);

    try {

      const timeRoundStarted = Date.now();

      await performProcess(mongooseConn, knex, collections, collectionsForHaku, oidsToProcess)
      console.log(`\n\nFinished processing ${oidsToProcess.length} haku or hakukohde. Total duration in seconds ${Math.round((Date.now() - timeRoundStarted) / 1000)}`);  

    } catch (error) {
      console.error('Caught exception while trying to process oids');
      console.error(error);
    }

    await sleep(SLEEP_TIME_SECONDS);
  } while (oidsToProcess.length > 0)

  process.exit(0);

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
      const errorWithMessageAndStack = `${error.message}, \n\n stack: ${error.stack?.toString()}`;
      await updateMigrationRow(knex, oid, false, errorWithMessageAndStack, null);
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
    console.log(`Fetching haku data took ${Math.round((Date.now() - timeStarted) / 1000)} seconds for haku ${hakuOid}`);
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
    if (collection.collectionName == 'Valinnanvaihe') {
      await copyValinnanVaihe(mongooseConn, trx, collection, rows);
    } else {
      const filledRows = await fillMongoObject({collections, tableName: collection.tableName, rows, mongooseConn});

      await putToPostgres({
        trx,
        collections,
        tableName: collection.tableName,
        rows: filledRows
      });
    }

  }
  const totalSeconds = Math.round((Date.now() - timeStarted) / 1000);
  console.log(`Took ${totalSeconds} seconds to copy hakukohde data for ${hakukohdeOid}`);
  return totalSeconds;
}


async function copyValinnanVaihe(mongooseConn, trx, collection, rows) {
  const idsMap = await putToPostgres({
    trx,
    collections: [collection],
    tableName: collection.tableName,
    rows
  });

  const jonoCollection = collection.subCollection;

  for (const row of rows) {
    
    if (row[jonoCollection.parentField] && row[jonoCollection.parentField].length > 0) {
      const ids = row[jonoCollection.parentField].map(id => id.oid ? id.oid : id);
      let totalDescendants = 0;
      const vvId = idsMap.find(idEntry => idEntry.oldId = row._id.toString()).newId;

      for (const vtpJonoId of ids) {
        const jono = await getFromMongoObjects(mongooseConn, jonoCollection.collectionName, [vtpJonoId]);
  
        const filledJono = await fillMongoObject({
          collections: [jonoCollection], 
          tableName: jonoCollection.tableName, 
          rows: jono, 
          mongooseConn});
        
        const fakeParentRow = {valintatapajonot: filledJono}
        totalDescendants += await storeSubCollection(trx, fakeParentRow, vvId, jonoCollection);
      }
  
      console.log(`Inserted ${ids.length} rows to Valintatapajono table with ${totalDescendants} number of descendants`);
    }

  }
  

} 

