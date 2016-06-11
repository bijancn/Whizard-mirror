import * as cuts from './cuts';
import * as models from './models';
import * as alias from './alias';
import * as process from './process';
import * as simulate from './tabs.simulate';
import * as constructSindarin from './constructSindarin';

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


  const Cuts = cuts.cutsClass.getCutsArray();
  const NewLineStarter = '\n\t and ';
  if (Cuts.length > 0) {
    let CutsRHS = '';
    for (let i = 0; i < Cuts.length; i++) {
      CutsRHS += Cuts[i] + NewLineStarter;
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
  cuts.clean();
  Scan.Clean();
  process.ProcessList = [];
  SimulateList = [];
  ScansList = [];
}

module.exports = {cleanAll, cuts, rebuildVariables};
