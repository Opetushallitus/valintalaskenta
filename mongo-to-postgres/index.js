import migrate from "./src/migrate.js";
import {connections, collections} from './settings.js';

migrate({connections, collections});

