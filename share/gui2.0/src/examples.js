/*
 * Pair Production ee->WW
 */
$(".ex-eeww").click(function() {

  /* Clean current settings */
  cleanAll();
  AddProcess('"e+", "e-"', '"W+", "W-"');
  ProcessList[0].setSqrts(500);
  MessageGUI("Example loaded.", "alert-success");
});
