import migrate from "./src/migrate.js";
import {connections, collections, collectionsForHaku} from './settings.js';

migrate({connections, collections, collectionsForHaku});

