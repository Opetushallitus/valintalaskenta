#mongo-to-postgres

Script that is used to migrate data from mongo to postgres.


## Usage

Create pass.js file with (it is in .gitignore):

`export const mongoPass = "passwordToMongo";
export const possuPass = "passwordToPostgres";`

Run to migrate data older than year 2022:
`npm start`

Run to migrate data newer than year 2021 (use this in real environments after the older data has been migrated):
`npm start useAfter`