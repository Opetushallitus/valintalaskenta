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