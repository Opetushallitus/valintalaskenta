import { v4 as uuidv4 } from 'uuid'

const copyHistoria = async (trx, historia, tunniste) => {
  await trx('jarjestyskriteerihistoria')
    .insert({historia, tunniste});
}

const handleJsonField = async (trx, collectionName, field, subRow ) => {
  if (collectionName === 'Jonosija' && field[0] === 'jarjestyskriteeritulokset') {
    const tulokset = [];
    for (const tulos of subRow[field[0]]) {
      if (tulos.historia) {
        const tunniste = uuidv4();
        await copyHistoria(trx, tulos.historia, tunniste);
        tulokset.push(Object.assign({}, tulos, {historia: tunniste}));
      } else {
        tulokset.push(tulos);
      }
      
    };
    return `{"${field[0]}":${JSON.stringify(tulokset)}}`;
  }
  return `{"${field[0]}":${JSON.stringify(subRow[field[0]])}}`;
}


export const storeSubCollection = async (trx, row, parentId, sub) => {
  const { tableName, collectionName, foreignKey, ordered, getMoreFieldsToAddFn,
    fieldsToCopy, parentField, jsonFields } = sub;
  const innerSub = sub.subCollection;
  const subs = row[parentField];

  if (!subs || !subs.length) {
    return 0;
  }

  let totalObjects = subs.length;

  for (let i = 0; i < subs.length; i ++) {
    let subRow = subs[i];

    const rowToInsert = {};

    for (const field of fieldsToCopy) {
      if (jsonFields && jsonFields.includes(field[0]) && subRow[field[0]]) {
        rowToInsert[field[1]] = await handleJsonField(trx, collectionName, field, subRow);
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

    rowToInsert[foreignKey] = parentId;

    const newId = uuidv4();
    rowToInsert.id = newId;

    await trx(tableName)
      .insert(rowToInsert);

    if (innerSub) {
      totalObjects += await storeSubCollection(trx, subRow, newId, innerSub);
    }
  }
  return totalObjects;
};

/**
   * Insert data to destination table
   * @param {object} trx - knex transaction object
   * @param {Array} collections - Array of collections
   * @param {string} tableName - Table name
   * @param {string} rows - Objects to insert
   */
export default async ({ trx, collections, tableName, rows }) => {
  const { fieldsToCopy, subCollection, getMoreFieldsToAddFn, jsonFields, fetchSubCollectionInBits } =
    collections.find(c => c.tableName === tableName);

  const idsMap = [];

  let totalDescendants = 0;

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

    const newId = uuidv4();
    rowToInsert.id = newId;

    if (getMoreFieldsToAddFn) {
      Object.assign(rowToInsert, getMoreFieldsToAddFn(currentRow));
    }

     await trx(tableName)
       .insert(rowToInsert);

    idsMap.push({ oldId, newId });

    if (!!subCollection && !fetchSubCollectionInBits) {
      totalDescendants += await storeSubCollection(trx, currentRow, newId, subCollection);
    }

  }

  console.log(`Inserted ${rows.length} rows to "${tableName}" table with ${totalDescendants} number of descendants`);

  return idsMap;
};
