import mongoose from 'mongoose';
import Knex from 'knex';
import getFromMongo from './get-from-mongo.js';
import putToPostgres from './put-to-postgres.js';

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

  //TODO: fetch these from control table
  // Control table: {hakuOid, hakukohdeOid, status (SUCCESS, FAIL, null), reason_for_fail, osallistumisetHandled};
  const oidsToProcess = [
    {haku: '1.2.246.562.29.00000000000000002821'}, 
    {hakukohde: '1.2.246.562.20.00000000000000004724'},
    {hakukohde: '1.2.246.562.20.45279263003'}];

  performProcess(mongooseConn, knex, collections, collectionsForHaku, oidsToProcess)
    .then(() => console.log('Finished successfully.'))
    .catch((err) => console.error(err))
    .finally(() => process.exit(0));
};

async function performProcess(mongooseConn, knex, collections, collectionsForHaku, oidsToProcess) {
  for (const oid of oidsToProcess) {
    if (oid.haku) {
      await copyHakuData(mongooseConn, knex, collectionsForHaku, oid.haku);
    } else {
      await copyHakukohdeData(mongooseConn, knex, collections, oid.hakukohde);
    }
  }
}

async function copyHakuData(mongooseConn, knex, collections, hakuOid) {
  const timeStarted = Date.now();

  for (const collection of collections) {
    const rows = await getFromMongo(mongooseConn, collection.collectionName, hakuOid, true);
    await putToPostgres({
      knex,
      collections,
      tableName: collection.tableName,
      rows,
      mongooseConn
    });
  }
  console.log(`Took ${(Date.now() - timeStarted) / 1000} seconds to copy haku data for ${hakuOid}`);
}

async function copyHakukohdeData(mongooseConn, knex, collections, hakukohdeOid) {
  const timeStarted = Date.now();

  for (const collection of collections) {
    const rows = await getFromMongo(mongooseConn, collection.collectionName, hakukohdeOid);
    await putToPostgres({
      knex,
      collections,
      tableName: collection.tableName,
      rows,
      mongooseConn
    });
  }
  console.log(`Took ${(Date.now() - timeStarted) / 1000} seconds to copy hakukohde data for ${hakukohdeOid}`);
}


