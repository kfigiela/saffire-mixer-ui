'use strict';

const path = require('path');
const app = require('electron').app;
const root = require('electron-root-path').rootPath;

const { getAppPath } = app;
const isPackaged =
  process.mainModule.filename.indexOf('app.asar') !== -1;

const binariesPath =
  isPackaged
    ? path.join(root, './Contents/Resources/bin')
    : path.join(root, './build/bin');

exports.serverPath = path.resolve(path.join(binariesPath, './saffire-mixer'));
