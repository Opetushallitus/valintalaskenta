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

export const getDistinctHakukohteetAndHaut = async (mongooseConn) => {
  let Model = Models['Valinnanvaihe'];
  if (!Model) {
    Model = mongooseConn.model('Valinnanvaihe',
      new mongoose.Schema({}, { collection: 'Valinnanvaihe'})
    );
    Models['Valinnanvaihe'] = Model;
  }

  const result = await Model.find({})
    .sort({createdAt: 1}).exec();
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