// This was just lying around in index.ejs. Disect and put in proper modules

//  Hiding whatever needed
$(".simulate-right, .integrate-right").hide();

/*
 *  Constructing models list
 */
var Models = fillModelList();

var ToolbarColumns = 4;
for(var k = 0; k < Models.length; k += ToolbarColumns)
{
  $("#pop_models").append('<div class="row">');
  for(var i = k; i < k + ToolbarColumns; i++)
  {
    var modelName = (Models[i] === undefined) ? "&nbsp;" : Models[i].modelName;
    var modelDescription = (Models[i] === undefined) ? "&nbsp;" : Models[i].description;
    $("#pop_models").append('<div class="col-md-3"><a href="javascript:;" title="' + modelDescription + '" class="model">' + modelName + '</a></div>');
  }
  $("#pop_models").append('</div>');
}

// Generate process list to choose setups from
function DisplayProcessList (ProcessList) {
  /*
   * Constructing integration list
   */
  $("#integrate-process-list").empty();
  for (var i=0; i<ProcessList.length; i++) {
    if (ProcessList[i] === null) continue;
    var ip1 = i+1;
    var Name = T(constructTex(ProcessList[i].Name()), ProcessList[i].Name());
    $("#integrate-process-list").append(
        '<a href="#" class="list-group-item process-entry" process-id="' + ip1 + '">' +
        Name + '</a>');
  }

  /*
   *  Constructing simulation list
   */
  $("#simulate-process-list").empty();
  for(var i = 0; i < ProcessList.length; i++)
  {
    if (ProcessList[i] === null) continue;
    var CSSClass = SimulateList[i].status ? "label-success": "label-default";
    var Text = SimulateList[i].status ? "On" : "Off";
    var Name = T(constructTex(ProcessList[i].Name()), ProcessList[i].Name());
    $("#simulate-process-list").append('<a href="#" class="list-group-item process-entry-sim" process-id="' + i + '">' +
        Name + '<br><span id="proc_indicator_'+i+'" class="label '+CSSClass+'">'+ Text +'</span></a>');
  }

  /*
   *  Constructing process list for TABS:scan
   */
  $("#scan-process-list").empty();
  for(var i = 0; i < ProcessList.length; i++)
  {
    if (ProcessList[i] === null) continue;
    var CSSClass = ScansList[i].status ? "label-success": "label-default";
    var Text = ScansList[i].status ? "On" : "Off";
    var Name = T(constructTex(ProcessList[i].Name()), ProcessList[i].Name());
    $("#scan-process-list").append('<a href="#" class="list-group-item process-entry-scan" process-id="' + i + '">' +
        Name + '<br><span id="proc_indicator_scan_'+i+'" class="label '+CSSClass+'">'+Text+'</span></a>');
  }

  /*
   * If no process added, suggest adding one
   */
  if (ProcessList.filter(function(value) { return value !== null }).length == 0) {
    $("#simulate-process-list").html("Please add a process.");
    $("#integrate-process-list").html("Please add a process.");
    $("#scan-process-list").html("Please add a process.");
    $(".simulate-right, .integrate-right, .scan-right").hide();
  }
}

function CleanAll() {
  $('input[type="text"]').val('');
  $('#conf-additional').val('');
  CleanAlias();
  cuts.Clean();
  Scan.Clean();
  ProcessList = [];
  SimulateList = [];
  ScansList = [];
}

function RebuildPreviewTab()
{
  rebuildVariables();
  $("#preview").html("<pre>" + SindarinScript + "</pre>");
}

/*
 * Working variables
 */

// Contains sindarin script
var SindarinScript = "";
var activeProcessId = -1;
var WhizRunning = false;


function rebuildVariables() {
  var SindarinList = new Array();

  //if ($("#conf-additional").val() )
  //  SindarinList.push (new SindarinCommand ( $("#conf-additional").val() ));

  //SindarinList.push (new SindarinAssignment ("model", $("#conf-model").text()));
  var model = new SindarinModel ($("#conf-model").text());
  SindarinList.push (new SindarinModelData (model));

  //Additional code
  if ($("#conf-additional").val()) {
    var AdditionalCode = new SindarinAdditionalCode($("#conf-additional").val() );
    SindarinList.push(AdditionalCode);
  }

  /* Only use the field if process list is empty */

  for(var i = 0; i < ProcessList.length; i++)
    if (ProcessList[i] !== null) SindarinList.push (ProcessList[i]);


  if ($("#conf-beams").val() )
    SindarinList.push (new SindarinAssignment ("beams", $("#conf-beams").val() + " => " + $("#conf-pdf").text()));

  /*
   *  Add Cuts (cuts.js)
   */
  var Cuts = cuts.getCutsArray();
  var NewLineStarter = '\n\t and ';
  if (Cuts.length > 0)
  {
    var CutsRHS = "";
    for(var i = 0; i < Cuts.length; i++)
      CutsRHS += Cuts[i] + NewLineStarter;
    CutsRHS = CutsRHS.substring(0, CutsRHS.length - NewLineStarter.length);

    SindarinList.push ( new SindarinCuts(CutsRHS) );
    //SindarinList.push (new SindarinAssignment ("cuts", CutsRHS));
  }

  /*
   *   Access Scans data
   */
  ExtAssignScans();



  SindarinList = SindarinList.concat(ExternalSindarinList);
  SindarinList = SindarinList.concat(SimulateList); ///!

  var a = new SindarinGenerator (SindarinList);
  SindarinScript = a.construct ();
}

/*
 * Creates a message box with appropriate style class
 * style = [alert-success, alert-warning, alert-danger]
 */
function MessageGUI(str, style)
{
  $("#controller").after('<div id="gui-box" class="alert ' + style + ' alert-dismissible" role="alert"><button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button><p id="gui-message">'+str+'</p></div>' );
}

$(document).ready(function() {
  $(".outputcontainer").hide();
  $("#pbar").hide();
  $("#form-events, #form-calls, #form-iterations").hide();
  $("#gui-box").hide();

  $("#button_alias").click(function() {

    /* Checking if both fields are non-empty */
    if ($("#conf-alias-lhs").val() && $("#conf-alias-rhs").val()) {

      AddAlias($("#conf-alias-lhs").val(), $("#conf-alias-rhs").val());
      rebuildAliasList();

      MessageGUI("New alias is added.", "alert-success");
      $("#conf-alias-lhs").val("");
      $("#conf-alias-rhs").val("");
    }
  });

  /*
   * Button: Add process
   */
  $("#button-add-process").click(function() {
    /* Checking if process input non empty */
    if ($("#conf-process-in").val() && $("#conf-process-out").val()) {
      AddProcess(parseParticleNameString($("#conf-process-in").val()),
        parseParticleNameString($("#conf-process-out").val()));


      MessageGUI("New process is added.", "alert-success");
    } else {
      var incoming_missing = "";
      var outgoing_missing = "";
      if (!$("#conf-process-in").val()) incoming_missing = "No incoming particles";
      if (!$("#conf-process-out").val()) outgoing_missing = "No outgoing particles";
      MessageGUI("Adding process failed! " + incoming_missing + " " +
        outgoing_missing, "alert-danger");
    }
  });

  /*
   *  Mini-button: Remove process
   */
  $(document).on("click", ".process-remove", function() {
    var id = $(this).attr("process-id");

    /*
     *  Nescessary to remove process element entirely.
     */
    ProcessList[id] = null;
    ProcessList.splice(id, 1); // No longer keeping nulls in the array
    rebuildProcessList();

    removeSimulateElement(id);
  });

  /*
   * Integrate checked, show #form-iterations and #for-calls
   */
  $("#conf-integrate").change(function() {
    if ($(this).prop('checked')) {
      $("#form-iterations").fadeIn("fast");
      $("#form-calls").fadeIn("fast");
    } else {
      $("#form-iterations").fadeOut("fast");
      $("#form-calls").fadeOut("fast");
    }
  });

  $("#conf-int-nlo").change(function() {
    try {
      if (activeProcessId < 0) throw ("Please select a process");
      if ($(this).prop('checked')) {
        ProcessList[activeProcessId].setNlo (true);
      } else {
        ProcessList[activeProcessId].setNlo (false);
      }
    } catch (err) {
      MessageGUI(err, "alert-danger");
    }
  });

  $("#conf-int-sqrts").change(function() {
    try {
      if (activeProcessId < 0) throw ("Please select a process");
      ProcessList[activeProcessId].setSqrts ($(this).val());
    } catch (err) {
      MessageGUI(err, "alert-danger");
    }
  });

  $("#conf-int-itt").change(function() {
    try {
      if (activeProcessId < 0) throw ("Please select a process");
      ProcessList[activeProcessId].setNIter ($(this).val());
    } catch (err) {
      MessageGUI(err, "alert-danger");
    }
  });

  $("#conf-int-cpi").change(function() {
    try {
      if (activeProcessId < 0) throw ("Please select a process");
      ProcessList[activeProcessId].setNCalls ($(this).val());
    } catch (err) {
      MessageGUI(err, "alert-danger");
    }
  });


  $(document).on("click", ".process-entry", function () {
    activeProcessId = $(this).attr("process-id") - 1;
    p = ProcessList[activeProcessId];
    if (!p.isNlo ()) {
      $("#conf-int-nlo").prop('checked', false);
    } else {
      $("#conf-int-nlo").prop('checked', true);
    }
    $("#conf-int-itt").val (p.getNIter());
    $("#conf-int-cpi").val (p.getNCalls());
    $("#conf-int-sqrts").val (p.getSqrts());

    $(".integrate-right").fadeIn("fast");
  });

  $(document).on("click", ".process-entry-sim", function () {
    activeProcessId = $(this).attr("process-id");
    p = SimulateList[activeProcessId];

    //Fill simulate fields
    $("#conf-sim-sim").prop('checked', p.getStatus());
    $("#conf-sim-events").val(p.getEvents());

    //Fill histogram fields
    Simulate.FillHistogramFieldsHTML();

    //Process selected show right column
    $(".simulate-right").fadeIn("fast");

  });

  /*
   * Simulate checked, show form-events
   */
  $("#conf-simulate").change(function() {
    if ($(this).prop('checked'))
    $("#form-events").fadeIn("fast");
    else
    $("#form-events").fadeOut("fast");
  });

  /*
   * Tab preview clicked, generate script
   */
  $("#tab_button_preview").click(function() {
    rebuildVariables();
    RebuildPreviewTab();
  });

  /*
   *  Changing tab, rebuild process list
   */
  $("#tab_button_integrate, #tab_button_simulate, #tab_button_scan").click(function() {
    DisplayProcessList (ProcessList);
  });

  /*
   * Tab Cuts clicked, generate particles list
   */
  $("#tab_button_cuts").click(function() {
    cuts.RebuildParticlesHTML();
  });

  /*
   * Tab Simulate clicked, generate particles popup list
   */
  $("#tab_button_simulate").click(function() {
    Simulate.RebuildParticlesHTML();
  });

  /*
   * Clicking on the model
   */
  $(document).on("click", ".model", function() {
    $("#conf-model").html($(this).text() + ' <span class="caret"></span>');
  });

  /*
   *  Remove Alias
   */
  $(document).on("click", ".alias-remove", function() {
    var id = $(this).attr("alias-id");
    ExternalSindarinList.splice(id, 1);
    rebuildAliasList();
  });

  /*
   * Button: Save Sindarin
   */
  $(".savesin").click(function() {
    rebuildVariables();
    $.post('/savesin', { src: SindarinScript }, function(data) {
      MessageGUI(data, "alert-success");
    });
  });

  /*
   * Button: Run Whizard
   */
  $(".runwhiz").click(function() {
    //Run option: [--rebuild-events, --rebuild-grids, --rebuild]
    var option = (typeof $(this).attr("opt") != 'undefined')? $(this).attr("opt") : "";

    //Animation
    $("#pbar").show();
    $("#whizoutput").fadeOut("fast");
    $(".outputcontainer").fadeOut("fast");

    //Functionality
    $(".runwhiz, .runarrow").attr("disabled", "disabled");
    rebuildVariables();
    WhizRunning = true;
    MonitorLogChanges();

    $.post('/runwhiz', { src: SindarinScript, option: option }, function(data) {
      //Animation
      $(".outputcontainer").fadeIn("fast");
      $("#pbar").fadeOut("fast");

      //Functionality
      WhizRunning = false;
      $(".runwhiz, .runarrow").removeAttr("disabled");
      /* Display output whizard file */
      $("#whizoutput").load( "whizard.log" ).fadeIn("fast");
      /* Display pdf (assuming there exists one for now) */
      $("#out_hist").html('<embed src="whizard_analysis.pdf" width="100%" height="700px">');

      /*
       *  Whiz->GUI error parser
       *  AM: check for other keywords, change method
       */
      var CritKeyword = 'FATAL ERROR:';
      var str = $.ajax({ url: "whizard.log",  async: false}).responseText;

      if (str.indexOf(CritKeyword) > -1) {
        var s=str.substring(str.lastIndexOf(CritKeyword),str.lastIndexOf('*')).replace(/\*/g,'');
        if (s)
          MessageGUI(s, 'alert-danger');
      }

    });

  });

  /* Design stuff */
  $('[rel=popover]').popover({
    html : true,
    content: function() {
      return $('#pop_models').html();
    }
  });

  $('[rel=popover_aliases]').popover({
    html : true,
    content: function() {
      return $('#pop_aliases').html();
    }
  });

  $('[rel=popover_process]').popover({
    html : true,
    content: function() {
      return $('#pop_process').html();
    }
  });

  /*
   * Popover for histogram
   */
  $('[data-toggle=popover_simulation_hist]').popover({
    html : true,
    content: function() {
      return $('#pop_sim_subevent').html();
    }
  });
});
