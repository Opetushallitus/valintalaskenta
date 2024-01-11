import { gunzipSync } from "zlib";
import {getFromMongoObject} from "./get-from-mongo.js";

const copyHistoria = async (mongooseConn, knex, historyId) => {
  const historiaDoc = await getFromMongoObject(mongooseConn, "Jarjestyskriteerihistoria", historyId);
  if (historiaDoc.historia) {
    return await trx('jarjestyskriteerihistoria')
      .insert(historiaDoc.historia, 'tunniste');
  } else {
    const historia = gunzipSync(historiaDoc.historiaGzip.buffer);
    return await knex('jarjestyskriteerihistoria')
      .insert({historia: historia.toString()}, 'tunniste');
  }
}

const handleJsonField = async (mongooseConn, trx, collectionName, field, subRow ) => {
  if (collectionName === 'Jonosija' && field[0] === 'jarjestyskriteeritulokset') {
    const tulokset = [];
    for (const tulos of subRow[field[0]]) {
      const historia = await copyHistoria(mongooseConn, trx, tulos.historia);
      tulokset.push(Object.assign({}, tulos, {historia: historia[0].tunniste}));
    };
    return `{"${field[0]}":${JSON.stringify(tulokset)}}`;
  }
  return `{"${field[0]}":${JSON.stringify(subRow[field[0]])}}`;
}


const copySubCollection = async (mongooseConn, trx, row, parentId, sub) => {
  const { tableName, collectionName, foreignKey, ordered, getMoreFieldsToAddFn,
    fieldsToCopy, parentField, jsonFields, embbeddedCollection } = sub;
  const innerSub = sub.subCollection;
  const subIds = row[parentField];

  for (let i = 0; i < subIds.length; i ++) {
    let subRow;
    if (!embbeddedCollection) {
      const idPropery = subIds[i].oid ? subIds[i].oid : subIds[i];
      subRow = await getFromMongoObject(mongooseConn, collectionName, idPropery);
    } else {
      subRow = subIds[i];
    }

    const rowToInsert = {};

    for (const field of fieldsToCopy) {
      if (jsonFields && jsonFields.includes(field[0]) && subRow[field[0]]) {
        rowToInsert[field[1]] = await handleJsonField(mongooseConn, trx, collectionName, field, subRow);
      } else {
        rowToInsert[field[1]] = subRow[field[0]];
      }
    }

    if (ordered) {
      rowToInsert[ordered] = i;
    }

    if (getMoreFieldsToAddFn) {
      Object.assign(rowToInsert, getMoreFieldsToAddFn(subRow));
    }

    rowToInsert[foreignKey] = parentId[0].id;

    const newId = await trx(tableName)
      .insert(rowToInsert, 'id');

    if (innerSub) {
      await copySubCollection(mongooseConn, trx, subRow, newId, innerSub);
    }
  }
};


/**
   * Insert data to destination table
   * @param {object} trx - knex transaction object
   * @param {Array} collections - Array of collections
   * @param {string} tableName - Table name
   * @param {string} rows - Objects to insert
   * @param {object} mongooseConn - mongoose object
   */
export default async ({ trx, collections, tableName, rows, mongooseConn }) => {
  const { fieldsToCopy, subCollection, getMoreFieldsToAddFn, jsonFields } =
    collections.find(c => c.tableName === tableName);

  const idsMap = [];

  for (const currentRow of rows) {

    const oldId = currentRow._id.toString();

    const rowToInsert = fieldsToCopy.reduce((prev, field) => {
      if (jsonFields && jsonFields.includes(field[0]) && currentRow[field[0]]) {
        prev[field[1]] = `{"${field[1]}":${JSON.stringify(currentRow[field[0]])}}`
      } else {
      prev[field[1]] = currentRow[field[0]];
      }
      return prev;
    }, {})

    if (getMoreFieldsToAddFn) {
      Object.assign(rowToInsert, getMoreFieldsToAddFn(currentRow));
    }

    // insert current row
     const newId = await trx(tableName)
       .insert(rowToInsert, 'id');

    // save id mapping
    idsMap.push({ oldId, newId: newId[0] });

    if (!!subCollection) {
      await copySubCollection(mongooseConn, trx, currentRow, newId, subCollection);
    }

  }

  console.log(`Inserted ${rows.length} rows to "${tableName}" table`);

  return idsMap;
};
