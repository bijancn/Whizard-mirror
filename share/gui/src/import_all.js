import 'babel-polyfill';
import '../public/bootstrap.min';
import './generic';
const cuts = require('./cuts');
const guiconfig = require('./guiconfig');
import './constructSindarin';
import './models';
import './examples';
import './alias';
import './process';
import './scan';
import './integration';
const simulate = require('./simulate');
import './index_dump';

cuts.setupJquery();
simulate.setupJquery();
