import { gunzipSync } from "zlib";
import {getFromMongoObject} from "./get-from-mongo.js";

const copyHistoria = async (mongooseConn, knex, historyId) => {
  const historiaDoc = await getFromMongoObject(mongooseConn, "Jarjestyskriteerihistoria", historyId);
  if (historiaDoc.historia) {
    return await knex('jarjestyskriteerihistoria')
      .returning('tunniste')
      .insert(historiaDoc.historia);
  } else {
    const historia = gunzipSync(historiaDoc.historiaGzip.buffer);
    return await knex('jarjestyskriteerihistoria')
      .returning('tunniste')
      .insert({historia: historia.toString()});
  }
}

const handleJsonField = async (mongooseConn, knex, collectionName, field, subRow ) => {
  if (collectionName === 'Jonosija' && field[0] === 'jarjestyskriteeritulokset') {
    const tulokset = [];
    for (const tulos of subRow[field[0]]) {
      const historia = await copyHistoria(mongooseConn, knex, tulos.historia);
      tulokset.push(Object.assign({}, tulos, {historia: historia[0].tunniste}));
    };
    return `{"${field[0]}":${JSON.stringify(tulokset)}}`;
  }
  return `{"${field[0]}":${JSON.stringify(subRow[field[0]])}}`;
}


const copySubCollection = async (mongooseConn, knex, row, parentId, sub) => {
  const { tableName, collectionName, foreignKey, ordered, fieldsToCopy, parentField, jsonFields } = sub;
  const innerSub = sub.subCollection;
  const subIds = row[parentField];

  for (let i = 0; i < subIds.length; i ++) {
    const idPropery = subIds[i].oid ? subIds[i].oid : subIds[i];
    const subRow = await getFromMongoObject(mongooseConn, collectionName, idPropery);

    const rowToInsert = {};

    for (const field of fieldsToCopy) {
      if (jsonFields && jsonFields.includes(field[0]) && subRow[field[0]]) {
        rowToInsert[field[1]] = await handleJsonField(mongooseConn, knex, collectionName, field, subRow);
      } else {
        rowToInsert[field[1]] = subRow[field[0]];
      }
    }

    if (ordered) {
      rowToInsert[ordered] = i;
    }

    rowToInsert[foreignKey] = parentId[0].id;

    const newId = await knex(tableName)
      .returning('id')
      .insert(rowToInsert);

    if (innerSub) {
      await copySubCollection(mongooseConn, knex, subRow, newId, innerSub);
    }
  }
};


/**
   * Insert data to destination table
   * @param {object} knex - knex object
   * @param {Array} collections - Array of collections
   * @param {string} tableName - Table name
   * @param {string} rows - Objects to insert
   */
export default async ({ knex, collections, tableName, rows, mongooseConn }) => {
  const { fieldsToCopy, subCollection, getMoreFieldsToAddFn, jsonFields } =
    collections.find(c => c.tableName === tableName);

  const idsMap = []; // array for identifiers maps
  for (const currentRow of rows) {

    // save and then delete Mongo _id
    const oldId = currentRow._id.toString();

    const rowToInsert = fieldsToCopy.reduce((prev, field) => {
      if (jsonFields && jsonFields.includes(field[0]) && currentRow[field[0]]) {
        prev[field[1]] = `{"${field[1]}":${JSON.stringify(currentRow[field[0]])}}`
      } else {
      prev[field[1]] = currentRow[field[0]];
      }
      return prev;
    }, {})

    console.log(rowToInsert);

    if (getMoreFieldsToAddFn) {
      Object.assign(rowToInsert, getMoreFieldsToAddFn(currentRow));
    }

    // insert current row
     const newId = await knex(tableName)
       .returning('id')
       .insert(rowToInsert);

    // save id mapping
    idsMap.push({ oldId, newId: newId[0] });

    if (!!subCollection) {
      await copySubCollection(mongooseConn, knex, currentRow, newId, subCollection);
    }

  }

  console.log(`Inserted ${rows.length} rows to "${tableName}" table`);

  return idsMap;
};
