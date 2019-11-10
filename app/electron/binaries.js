'use strict';

const path = require('path');
// const app = require('electron').app;
const root = require('electron-root-path').rootPath;

// const { getAppPath } = app;

exports.serverPath = path.resolve(path.join(root, './saffire-mixer'));
