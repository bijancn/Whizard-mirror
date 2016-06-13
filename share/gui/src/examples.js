const backend = require('./backend');
const process = require('./process');

// Pair Production ee->WW
$('.ex-eeww').click(() => {
  backend.cleanAll();
  process.addProcess('"e+", "e-"', '"W+", "W-"');
  process.ProcessList[0].setSqrts(500);
  backend.messageGUI("Example loaded.", "alert-success");
});
