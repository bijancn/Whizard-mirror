import 'babel-polyfill';
import '../public/bootstrap.min';
import './generic';
const cuts = require('./cuts');
// const guiconfig = require('./guiconfig');
const constructSindarin = require('./constructSindarin');
const models = require('./models');
import './examples';
const alias = require('./alias');
const process = require('./process');
const scan = require('./scan');
const simulation = require('./simulation');
import './index_dump';


export function rebuildVariables() {
  let SindarinList = [];
  const modelString = $('#conf-model').text();
  SindarinList.push(new models.SindarinModelData(modelString));

  if ($('#conf-additional').val()) {
    const AdditionalCode = new
      constructSindarin.SindarinAdditionalCode($('#conf-additional').val());
    SindarinList.push(AdditionalCode);
  }

  if ($('#conf-beams').val()) {
    SindarinList.push(new constructSindarin.SindarinAssignment('beams',
          $('#conf-beams').val() + ' => ' + $('#conf-pdf').text()));
  }

  const NewLineStarter = '\n\t and ';
  const cutsList = cuts.cutsClosure.getCutsArray();
  if (cutsList.length > 0) {
    let CutsRHS = '';
    for (let i = 0; i < cuts.Instance.length; i++) {
      CutsRHS += cutsList[i] + NewLineStarter;
    }
    CutsRHS = CutsRHS.substring(0, CutsRHS.length - NewLineStarter.length);
    SindarinList.push(new cuts.SindarinCuts(CutsRHS));
  }

  // Access Scans data
  process.extAssignScans();

  SindarinList = SindarinList.concat(alias.ExternalSindarinList);
  SindarinList = SindarinList.concat(simulation.SimulateList);

  const a = new constructSindarin.SindarinGenerator(SindarinList, process.ProcessList);
  return a.construct();
}


export function cleanAll() {
  $('input[type="text"]').val('');
  $('#conf-additional').val('');
  alias.cleanAlias();
  cuts.cutsClosure.clean();
  scan.Scan.clean();
  process.ProcessList = [];
  simulation.SimulateList = [];
  scan.ScansList = [];
}


// Creates a message box with appropriate style class
// style = [alert-success, alert-warning, alert-danger]
export function messageGUI(str, style) {
  $('#controller').after('<div id="gui-box" class="alert ' + style +
      ' alert-dismissible" role="alert"><button type="button" class="close" ' +
      'data-dismiss="alert" aria-label="Close">' +
      '<span aria-hidden="true">&times;</span></button><p id="gui-message">' +
      str + '</p></div>');
}


export function rebuildPreviewTab() {
  const SindarinScript = rebuildVariables();
  $('#preview').html('<pre>' + SindarinScript + '</pre>');
}


cuts.setupJquery();
simulation.setupJquery();
scan.setupJquery();
