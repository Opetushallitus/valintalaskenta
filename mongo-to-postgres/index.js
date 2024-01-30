import { argv } from 'node:process';
import migrate from "./src/migrate.js";
import {connections, collections, collectionsForHaku} from './settings.js';

const useAfter = !!argv.find(a => a == 'useAfter');

migrate({connections, collections, collectionsForHaku, useAfter});

