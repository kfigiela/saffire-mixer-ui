'use strict';

const path = require('path');
const root = require('electron-root-path').rootPath;

exports.serverPath = path.resolve(path.join(root, './saffire-mixer'));
