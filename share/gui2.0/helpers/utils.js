(function () {
  'use strict';
  var promisify = require("es6-promisify"),
      fs = require('fs');
  var mkdir = promisify(fs.mkdir, function (err) {
    if (err) {
      if (err.code === 'EEXIST') {
        return this.resolve('Folder ' + err.path + ' already exists');
      } else {
        return this.reject(err);
      }
    } else {
      return this.resolve('Successfully created folder');
    }
  });
  module.exports = { "mkdir": mkdir};
})();
