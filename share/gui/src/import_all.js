import 'babel-polyfill';
import '../public/bootstrap.min';
import './generic';
const cuts = require('./cuts');
// const guiconfig = require('./guiconfig');
import './constructSindarin';
import './models';
import './examples';
import './alias';
import './process';
const scan = require('./scan');
const simulation = require('./simulation');
import './index_dump';

cuts.setupJquery();
simulation.setupJquery();
scan.setupJquery();
