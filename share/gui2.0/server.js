(function () {
  'use strict';
  var express = require('express'),
      mongo = require('mongodb').MongoClient,
      promisify = require('es6-promisify'),
      log = require('js-logger'),
      utils = require('./helpers/utils'),
      guiconf = require('./controllers/guiconfig'); // This should parse a settings JSON
  var app = express();
  var mongoPort = 27017;

  require('es6-promise').polyfill;  // Remove as soon standard node has it
  log.useDefaults();
  log.setLevel(guiconf.logLevel);

  utils.mkdir(guiconf.whizardOutputDir)
    .then(function (success) {
      log.info(success);
      return utils.mkdir(guiconf.whizardOutputSin);
    })
    .then(log.info).catch(log.error);

  var startServer = function (db) {
    console.log('MongoDB successfully connected on port: ' + mongoPort);
    app.set('view engine', 'ejs');
    app.use('/public', express.static(process.cwd() + '/public'));
    app.use('/controllers', express.static(process.cwd() + '/controllers'));
    app.get('/', function (req, res) {
      res.render('index');
    });

    //app.set('port', guiconf.port);
    app.listen(guiconf.port, function () {
      console.log('Listening on port: ' + guiconf.port);
    });
  };

  promisify(mongo.connect)('mongodb://localhost:' + mongoPort + '/whizard')
    .then(function(mongodb){ startServer(mongodb); })
    .catch(console.log.bind(console));
} ()
);
