const express = require('express');
// let mongo = require('mongodb').MongoClient;
// const promisify = require('es6-promisify');
const log = require('js-logger');
const utils = require('./utils');
const guiconf = require('./guiconfig'); // This should parse a settings JSON/YAML
// const es6Promise = require('es6-promise');
const app = express();
// const mongoPort = 27017;
// es6Promise.polyfill();  // Remove as soon standard node has it
const startServer = () => {
  // console.log('MongoDB successfully connected on port: ' + mongoPort);
  app.set('view engine', 'ejs');
  app.use('/public', express.static(process.cwd() + '/public'));
  app.use('/browser', express.static(process.cwd() + '/browser'));
  app.get('/', (req, res) => {
    res.render('index');
  });

  // app.set('port', guiconf.port);
  app.listen(guiconf.port, () => {
    console.log('Listening on port: ' + guiconf.port);
  });
};
log.useDefaults();
log.setLevel(guiconf.logLevel);

utils.mkdir(guiconf.whizardOutputDir)
  .then((success) => {
    log.info(success);
    return utils.mkdir(guiconf.whizardOutputSin);
  })
  .then(log.info).catch(log.error);


startServer();
// It is probably better to save the state in a mongo db but it also adds a
// further dependency. For now disabled
// promisify(mongo.connect)('mongodb://localhost:' + mongoPort + '/whizard')
  // .then(function(mongodb) {
    // startServer(mongodb);
  // })
  // .catch(console.log.bind(console));
