import mongoose from 'mongoose';
import Knex from 'knex';
import getFromMongo from './get-from-mongo.js';
import putToPostgres from './put-to-postgres.js';

export default async ({ connections, collections }) => {
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

  performProcess(mongooseConn, knex, collections)
    .then(() => console.log('Finished successfully.'))
    .catch((err) => console.error(err))
    .finally(() => process.exit(0));
};

async function performProcess(mongooseConn, knex, collections) {
  for (const collection of collections) {
    const rows = await getFromMongo(mongooseConn, collection.collectionName, '1.2.246.562.20.00000000000000004724');
    console.log(rows.length);
    await putToPostgres({
      knex,
      collections,
      tableName: collection.tableName,
      rows,
      mongooseConn
    });
  }
}