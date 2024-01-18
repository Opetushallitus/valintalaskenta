import mongoose from 'mongoose';
import { gunzipSync } from "zlib";

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

export const getDistinctHakukohteetAndHaut = async (mongooseConn) => {
  let Model = Models['Valinnanvaihe'];
  if (!Model) {
    Model = mongooseConn.model('Valinnanvaihe',
      new mongoose.Schema({}, { collection: 'Valinnanvaihe'})
    );
    Models['Valinnanvaihe'] = Model;
  }

  const result = await Model.find({})
    .sort({createdAt: -1}).exec();
    //.where('createdAt').lt() // for start need to fetch only ones older than 2 years

  const distinctHakuOids = [];
  const distinctHakukohdeOids = [];
  const response = [];
  
  result.map((r) => r._doc).forEach(result => {
    if (distinctHakuOids.indexOf(result.hakuOid) < 0) {
      distinctHakuOids.push(result.hakuOid);
      response.push({haku: result.hakuOid});
    }

    if (distinctHakukohdeOids.indexOf(result.hakukohdeOid) < 0) {
      distinctHakukohdeOids.push(result.hakukohdeOid);
      response.push({haku: result.hakuOid, hakukohde: result.hakukohdeOid});
    }
  })

  return response;
}

export const getFromMongoObject = async (mongooseConn, collectionName, objectId) => {
  let Model = Models[collectionName];
  if (!Model) {
    Model = mongooseConn.model(collectionName,
      new mongoose.Schema({}, { collection: collectionName })
    );
    Models[collectionName] = Model;
  }
  
  const result = await Model.findById(objectId);
  return result._doc;
};










const copyHistoria = async (mongooseConn, historyId) => {
  const historiaDoc = await getFromMongoObject(mongooseConn, "Jarjestyskriteerihistoria", historyId);
  if (historiaDoc.historia) {
    return historiaDoc.historia;
  } else {
    const historia = gunzipSync(historiaDoc.historiaGzip.buffer);
    return historia.toString();
  }
}


const fillSubCollection = async (mongooseConn, row, sub) => {
  const { collectionName,
    fieldsToCopy, parentField, embbeddedCollection } = sub;
  const innerSub = sub.subCollection;
  const subIds = row[parentField];

  if (!subIds || !subIds.length) {
    return [];
  }

  let subObjs = [];

  for (let i = 0; i < subIds.length; i ++) {
    let subRow;
    if (!embbeddedCollection) {
      const idPropery = subIds[i].oid ? subIds[i].oid : subIds[i];
      subRow = await getFromMongoObject(mongooseConn, collectionName, idPropery);
    } else {
      subRow = subIds[i];
    }

    for (const field of fieldsToCopy) {
      if (collectionName === 'Jonosija' && field[0] === 'jarjestyskriteeritulokset') {
        for (const tulos of subRow[field[0]]) {
          tulos.historia = await copyHistoria(mongooseConn, tulos.historia);
        };
      }
    }

    if (innerSub) {
      const innerSubs = await fillSubCollection(mongooseConn, subRow, innerSub);
      subObjs.push(Object.assign(subRow, {[innerSub.parentField]: innerSubs}));
    } else {
      subObjs.push(subRow);
    }
  }
  return subObjs;
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