(function() {
  'use strict';
  var express = require('express');
  var mongo = require('mongodb').MongoClient;
  var promisify = require('es6-promisify');
  var log = require('js-logger');
  var utils = require('./helpers/utils');
  var guiconf = require('./guiconfig'); // This should parse a settings JSON/YAML
  var es6Promise = require('es6-promise');
  var app = express();
  var mongoPort = 27017;
  es6Promise.polyfill();  // Remove as soon standard node has it

  log.useDefaults();
  log.setLevel(guiconf.logLevel);

  utils.mkdir(guiconf.whizardOutputDir)
    .then(function(success) {
      log.info(success);
      return utils.mkdir(guiconf.whizardOutputSin);
    })
    .then(log.info).catch(log.error);

  var startServer = function(db) {
    console.log('MongoDB successfully connected on port: ' + mongoPort);
    app.set('view engine', 'ejs');
    app.use('/public', express.static(process.cwd() + '/../public'));
    app.use('/controllers', express.static(process.cwd()));
    app.get('/', function(req, res) {
      res.render('index');
    });

    // app.set('port', guiconf.port);
    app.listen(guiconf.port, function() {
      console.log('Listening on port: ' + guiconf.port);
    });
  };

  promisify(mongo.connect)('mongodb://localhost:' + mongoPort + '/whizard')
    .then(function(mongodb) {
      startServer(mongodb);
    })
    .catch(console.log.bind(console));
})()
;
