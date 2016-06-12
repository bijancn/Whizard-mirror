const cuts = require('./cuts');
const models = require('./models');
const alias = require('./alias');
const process = require('./process');
const simulate = require('./tabs.simulate');
const constructSindarin = require('./constructSindarin');

function rebuildVariables() {
  let SindarinList = [];
  const model = new models.SindarinModel($('#conf-model').text());
  SindarinList.push(new models.SindarinModelData(model));

  if ($('#conf-additional').val()) {
    const AdditionalCode = new
      constructSindarin.SindarinAdditionalCode($('#conf-additional').val());
    SindarinList.push(AdditionalCode);
  }

  /* Only use the field if process list is empty */

  for (let i = 0; i < process.ProcessList.length; i++) {
    if (process.ProcessList[i] !== null) SindarinList.push(process.ProcessList[i]);
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
  process.ExtAssignScans();

  SindarinList = SindarinList.concat(alias.ExternalSindarinList);
  SindarinList = SindarinList.concat(simulate.SimulateList);

  const a = new constructSindarin.SindarinGenerator(SindarinList);
  return a.construct();
}


function cleanAll() {
  $('input[type="text"]').val('');
  $('#conf-additional').val('');
  alias.cleanAlias();
  cuts.cutsClosure.clean();
  Scan.Clean();
  process.ProcessList = [];
  simulate.SimulateList = [];
  ScansList = [];
}

module.exports = {cleanAll, cuts, rebuildVariables};
