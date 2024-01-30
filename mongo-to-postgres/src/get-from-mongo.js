import mongoose from 'mongoose';

const Models = {};

/**
   * Get data from source collection
   * @param {string} mongooseConn - Connection to mongoose
   * @param {string} collectionName - Collection name
   * @param {string} hakukohdeOid - Hakukohde identifier
   * @return {Array} Retrieved objects
   */
export default async (mongooseConn, collectionName, hakukohdeOid, useHaku = false) => {

  let Model = Models[collectionName];
  if (!Model) {
    Model = mongooseConn.model(collectionName,
      new mongoose.Schema({}, { collection: collectionName })
    );
    Models[collectionName] = Model;
  }
  
  const result = await Model.find(useHaku? {hakuOid: hakukohdeOid}: {hakukohdeOid});
  return result.map((r) => {
    return r._doc;
  });
};

export const getDistinctHakukohteetAndHaut = async (mongooseConn, olderThan, newerThan = null) => {
  let Model = Models['Valinnanvaihe'];
  if (!Model) {
    Model = mongooseConn.model('Valinnanvaihe',
      new mongoose.Schema({}, { collection: 'Valinnanvaihe'})
    );
    Models['Valinnanvaihe'] = Model;
  }

  let result;

  if (newerThan != null) {
    result = await Model.find({createdAt: {$gt: newerThan}})
      .sort({createdAt: -1}).exec();
  } else {
    result = await Model.find({createdAt: {$lt: olderThan}})
      .sort({createdAt: -1}).exec();
  }

  const distinctHakuOids = [];
  const distinctHakukohdeOids = [];
  const response = [];
  
  result.map((r) => r._doc).forEach(result => {
    if (distinctHakuOids.indexOf(result.hakuOid) < 0) {
      distinctHakuOids.push(result.hakuOid);
      response.push({haku: result.hakuOid, createdAt: result.createdAt});
    }

    if (distinctHakukohdeOids.indexOf(result.hakukohdeOid) < 0) {
      distinctHakukohdeOids.push(result.hakukohdeOid);
      response.push({haku: result.hakuOid, hakukohde: result.hakukohdeOid, createdAt: result.createdAt});
    }
  })

  return response;
}

export const getFromMongoObjects = async (mongooseConn, collectionName, objectIds) => {
  let Model = Models[collectionName];
  if (!Model) {
    Model = mongooseConn.model(collectionName,
      new mongoose.Schema({}, { collection: collectionName })
    );
    Models[collectionName] = Model;
  }
  
  const result = await Model.find().where('_id').in(objectIds).exec();
  return result.map((r) => {
    return r._doc;
  });
};

const fillSubCollection = async (mongooseConn, row, sub) => {
  const { collectionName,
    fieldsToCopy, parentField, embbeddedCollection } = sub;
  const innerSub = sub.subCollection;
  const subIds = row[parentField];

  if (!subIds || !subIds.length) {
    return [];
  }

  let subs = [];

  let tuloksetField = fieldsToCopy.find(field => field[0] === 'jarjestyskriteeritulokset');

  if (!embbeddedCollection) {
    const ids = subIds.map(id => id.oid ? id.oid : id);
    subs = await getFromMongoObjects(mongooseConn, collectionName, ids);
  } else {
    subs = subIds;
  }

  let histories = [];

  for (let subRow of subs) {

    if (collectionName === 'Jonosija') {
      for (const tulos of subRow[tuloksetField[0]]) {
        if (tulos.historia) {
          histories.push(tulos.historia);
        }
      };
    }

    if (innerSub) {
      const innerSubs = await fillSubCollection(mongooseConn, subRow, innerSub);
      subRow[innerSub.parentField] = innerSubs;
    }
  }

  if (histories.length > 0 && collectionName === 'Jonosija') {
    const historyObjs = await getFromMongoObjects(mongooseConn, "Jarjestyskriteerihistoria", histories);
    for (let subObj of subs) {
      for (const tulos of subObj[tuloksetField[0]]) {
        tulos.historia = historyObjs.find(h => h._id.equals(tulos.historia));
      };
    }
  }

  return subs; //doing in mutable way as memory runs out with larger objects;
};

export const fillMongoObject = async ({ collections, tableName, rows, mongooseConn }) => {
  const { subCollection } =
    collections.find(c => c.tableName === tableName);

  const filledObjs = [];

  for (const currentRow of rows) {

    if (!!subCollection) {
      const filledChildren = await fillSubCollection(mongooseConn, currentRow, subCollection);
      filledObjs.push(Object.assign(currentRow, {[subCollection.parentField]: filledChildren})) 
    } else {
      filledObjs.push(currentRow);
    }

  }

  return filledObjs;
};