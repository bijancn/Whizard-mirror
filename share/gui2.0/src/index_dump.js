// This was just lying around in index.ejs
// TODO: (bcn 2016-06-11) Disect and put in proper modules

const models = require('./models');
const alias = require('./alias');
const backend = require('./backend');
const process = require('./process');
const simulate = require('./tabs.simulate.js');
const cuts = require('./cuts');
const ToolbarColumns = 4;
const Models = models.fillModelList();

//  Hiding whatever needed
$('.simulate-right, .integrate-right').hide();

for (let k = 0; k < Models.length; k += ToolbarColumns) {
  $('#pop_models').append('<div class="row">');
  for (let i = k; i < k + ToolbarColumns; i++) {
    const modelName = (Models[i] === undefined) ? '&nbsp;' : Models[i].modelName;
    const modelDescription = (Models[i] === undefined) ? '&nbsp;' : Models[i].description;
    $('#pop_models').append('<div class="col-md-3"><a href="javascript:;" title="'
        + modelDescription + '" class="model">' + modelName + '</a></div>');
  }
  $('#pop_models').append('</div>');
}

function RebuildPreviewTab() {
  SindarinScript = backend.rebuildVariables();
  $('#preview').html('<pre>' + SindarinScript + '</pre>');
}

/*
 * Working variables
 */

// Contains sindarin script
var SindarinScript = '';
var activeProcessId = -1;
var WhizRunning = false;


/*
 * Creates a message box with appropriate style class
 * style = [alert-success, alert-warning, alert-danger]
 */
function MessageGUI(str, style) {
  $('#controller').after('<div id="gui-box" class="alert ' + style +
      ' alert-dismissible" role="alert"><button type="button" class="close" ' +
      'data-dismiss="alert" aria-label="Close">' +
      '<span aria-hidden="true">&times;</span></button><p id="gui-message">' +
      str + '</p></div>');
}

$(document).ready(function() {
  $('.outputcontainer').hide();
  $('#pbar').hide();
  $('#form-events, #form-calls, #form-iterations').hide();
  $('#gui-box').hide();

  $('#button_alias').click(function() {

    /* Checking if both fields are non-empty */
    if ($('#conf-alias-lhs').val() && $('#conf-alias-rhs').val()) {

      alias.AddAlias($('#conf-alias-lhs').val(), $('#conf-alias-rhs').val());
      alias.rebuildAliasList();

      MessageGUI('New alias is added.', 'alert-success');
      $('#conf-alias-lhs').val('');
      $('#conf-alias-rhs').val('');
    }
  });

  /*
   * Button: Add process
   */
  $('#button-add-process').click(function() {
    /* Checking if process input non empty */
    if ($('#conf-process-in').val() && $('#conf-process-out').val()) {
      AddProcess(parseParticleNameString($('#conf-process-in').val()),
        parseParticleNameString($('#conf-process-out').val()));


      MessageGUI('New process is added.', 'alert-success');
    } else {
      var incoming_missing = '';
      var outgoing_missing = '';
      if (!$('#conf-process-in').val()) incoming_missing = 'No incoming particles';
      if (!$('#conf-process-out').val()) outgoing_missing = 'No outgoing particles';
      MessageGUI('Adding process failed! ' + incoming_missing + ' ' +
        outgoing_missing, 'alert-danger');
    }
  });

  /*
   *  Mini-button: Remove process
   */
  $(document).on('click', '.process-remove', function() {
    var id = $(this).attr('process-id');

    /*
     *  Nescessary to remove process element entirely.
     */
    process.ProcessList[id] = null;
    process.ProcessList.splice(id, 1); // No longer keeping nulls in the array
    rebuildProcessList();

    removeSimulateElement(id);
  });

  /*
   * Integrate checked, show #form-iterations and #for-calls
   */
  $('#conf-integrate').change(function() {
    if ($(this).prop('checked')) {
      $('#form-iterations').fadeIn('fast');
      $('#form-calls').fadeIn('fast');
    } else {
      $('#form-iterations').fadeOut('fast');
      $('#form-calls').fadeOut('fast');
    }
  });

  $('#conf-int-nlo').change(function() {
    try {
      if (activeProcessId < 0) throw ('Please select a process');
      if ($(this).prop('checked')) {
        process.ProcessList[activeProcessId].setNlo (true);
      } else {
        process.ProcessList[activeProcessId].setNlo (false);
      }
    } catch (err) {
      MessageGUI(err, 'alert-danger');
    }
  });

  $('#conf-int-sqrts').change(function() {
    try {
      if (activeProcessId < 0) throw ('Please select a process');
      process.ProcessList[activeProcessId].setSqrts ($(this).val());
    } catch (err) {
      MessageGUI(err, 'alert-danger');
    }
  });

  $('#conf-int-itt').change(function() {
    try {
      if (activeProcessId < 0) throw ('Please select a process');
      process.ProcessList[activeProcessId].setNIter ($(this).val());
    } catch (err) {
      MessageGUI(err, 'alert-danger');
    }
  });

  $('#conf-int-cpi').change(function() {
    try {
      if (activeProcessId < 0) throw ('Please select a process');
      process.ProcessList[activeProcessId].setNCalls ($(this).val());
    } catch (err) {
      MessageGUI(err, 'alert-danger');
    }
  });


  $(document).on('click', '.process-entry', function () {
    activeProcessId = $(this).attr('process-id') - 1;
    p = process.ProcessList[activeProcessId];
    if (!p.isNlo ()) {
      $('#conf-int-nlo').prop('checked', false);
    } else {
      $('#conf-int-nlo').prop('checked', true);
    }
    $('#conf-int-itt').val (p.getNIter());
    $('#conf-int-cpi').val (p.getNCalls());
    $('#conf-int-sqrts').val (p.getSqrts());

    $('.integrate-right').fadeIn('fast');
  });

  $(document).on('click', '.process-entry-sim', function () {
    activeProcessId = $(this).attr('process-id');
    p = simulate.SimulateList[activeProcessId];

    //Fill simulate fields
    $('#conf-sim-sim').prop('checked', p.getStatus());
    $('#conf-sim-events').val(p.getEvents());

    //Fill histogram fields
    simulate.Simulate.FillHistogramFieldsHTML();

    //Process selected show right column
    $('.simulate-right').fadeIn('fast');
  });

  /*
   * Simulate checked, show form-events
   */
  $('#conf-simulate').change(() => {
    if ($(this).prop('checked')) {
      $('#form-events').fadeIn('fast');
    } else {
      $('#form-events').fadeOut('fast');
    }
  });

  /*
   * Tab preview clicked, generate script
   */
  $('#tab_button_preview').click(function() {
    SindarinScript = backend.rebuildVariables();
    RebuildPreviewTab();
  });

  /*
   *  Changing tab, rebuild process list
   */
  $('#tab_button_integrate, #tab_button_simulate, #tab_button_scan').click(function() {
    process.DisplayProcessList (process.ProcessList);
  });

  /*
   * Tab Cuts clicked, generate particles list
   */
  $('#tab_button_cuts').click(() => {
    cuts.Instance.RebuildParticlesHTML();
  });

  /*
   * Tab Simulate clicked, generate particles popup list
   */
  $('#tab_button_simulate').click(() => {
    simulate.Simulate.RebuildParticlesHTML();
  });

  /*
   * Clicking on the model
   */
  $(document).on('click', '.model', function() {
    $('#conf-model').html($(this).text() + ' <span class="caret"></span>');
  });

  /*
   *  Remove Alias
   */
  $(document).on('click', '.alias-remove', () => {
    var id = $(this).attr('alias-id');
    alias.ExternalSindarinList.splice(id, 1);
    alias.rebuildAliasList();
  });

  /*
   * Button: Save Sindarin
   */
  $(".savesin").click(function() {
    SindarinScript = backend.rebuildVariables();
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
    SindarinScript = backend.rebuildVariables();
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
