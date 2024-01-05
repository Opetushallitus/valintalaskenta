import {getFromMongoObject} from "./get-from-mongo.js";


const copySubCollection = async (mongooseConn, knex, row, parentId, sub) => {
  const { tableName, collectionName, foreignKey, ordered, fieldsToCopy, parentField, jsonFields } = sub;
  const innerSub = sub.subCollection;
  const subIds = row[parentField];

  for (let i = 0; i < subIds.length; i ++) {
    const idPropery = subIds[i].oid ? subIds[i].oid : subIds[i];
    const subRow = await getFromMongoObject(mongooseConn, collectionName, idPropery);
    const rowToInsert = fieldsToCopy.reduce((prev, field) => {
      if (jsonFields && jsonFields.includes(field[0])) {
        prev[field[1]] = JSON.stringify(subRow[field[0]]);
      } else {
        prev[field[1]] = subRow[field[0]];
      }
      return prev;
    }, {})

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
   * @param {object} kenx - knex object
   * @param {Array} collections - Array of collections
   * @param {string} tableName - Table name
   * @param {string} rows - Objects to insert
   */
export default async ({ knex, collections, tableName, rows, mongooseConn }) => {
  const { fieldsToCopy, subCollection } =
    collections.find(c => c.tableName === tableName);

  const idsMap = []; // array for identifiers maps
  for (const currentRow of rows) {

    // save and then delete Mongo _id
    const oldId = currentRow._id.toString();

    const rowToInsert = fieldsToCopy.reduce((prev, field) => {
      prev[field[1]] = currentRow[field[0]];
      return prev;
    }, {})

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
