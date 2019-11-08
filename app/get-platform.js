'use strict';

const os = require('os');

exports.getPlatform = function () {
  switch (os.platform()) {
    case 'aix':
    case 'sunos':
    case 'freebsd':
    case 'linux':
    case 'openbsd':
    case 'android':
      return 'linux';
    case 'darwin':
      return 'mac';
    case 'win32':
      return 'win';
  }
};
