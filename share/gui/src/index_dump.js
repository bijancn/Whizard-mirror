// This was just lying around in index.ejs
// TODO: (bcn 2016-06-11) Disect and put in proper modules

const generic = require('./generic');
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

      backend.messageGUI('New alias is added.', 'alert-success');
      $('#conf-alias-lhs').val('');
      $('#conf-alias-rhs').val('');
    }
  });

  // Button: Add process
  $('#button-add-process').click(() => {
    // Checking if process input non empty
    if ($('#conf-process-in').val() && $('#conf-process-out').val()) {
      process.addProcess(generic.parseParticleNameString($('#conf-process-in').val()),
        generic.parseParticleNameString($('#conf-process-out').val()));
      backend.messageGUI('New process is added.', 'alert-success');
    } else {
      let incomingMissing = '';
      let outgoingMissing = '';
      if (!$('#conf-process-in').val()) incomingMissing = 'No incoming particles';
      if (!$('#conf-process-out').val()) outgoingMissing = 'No outgoing particles';
      backend.messageGUI('Adding process failed! ' + incomingMissing + ' and ' +
        outgoingMissing, 'alert-danger');
    }
  });

  /*
   *  Mini-button: Remove process
   */
  $(document).on('click', '.process-remove', () => {
    const id = $(this).attr('process-id');
    // Nescessary to remove process element entirely.
    process.ProcessList[id] = null;
    process.ProcessList.splice(id, 1); // No longer keeping nulls in the array
    process.rebuildProcessList();
    simulate.removeSimulateElement(id);
  });

  /*
   * Integrate checked, show #form-iterations and #for-calls
   */
  $('#conf-integrate').change(() => {
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
      if (simulate.activeProcessId < 0) throw ('Please select a process');
      if ($(this).prop('checked')) {
        process.ProcessList[simulate.activeProcessId].setNlo (true);
      } else {
        process.ProcessList[simulate.activeProcessId].setNlo (false);
      }
    } catch (err) {
      backend.messageGUI(err, 'alert-danger');
    }
  });

  $('#conf-int-sqrts').change(function() {
    try {
      if (simulate.activeProcessId < 0) throw ('Please select a process');
      process.ProcessList[simulate.activeProcessId].setSqrts ($(this).val());
    } catch (err) {
      backend.messageGUI(err, 'alert-danger');
    }
  });

  $('#conf-int-itt').change(function() {
    try {
      if (simulate.activeProcessId < 0) throw ('Please select a process');
      process.ProcessList[simulate.activeProcessId].setNIter ($(this).val());
    } catch (err) {
      backend.messageGUI(err, 'alert-danger');
    }
  });

  $('#conf-int-cpi').change(function() {
    try {
      if (simulate.activeProcessId < 0) throw ('Please select a process');
      process.ProcessList[simulate.activeProcessId].setNCalls ($(this).val());
    } catch (err) {
      backend.messageGUI(err, 'alert-danger');
    }
  });


  $(document).on('click', '.process-entry', function () {
    simulate.activeProcessId = $(this).attr('process-id') - 1;
    const p = process.ProcessList[simulate.activeProcessId];
    if (!p.isNlo()) {
      $('#conf-int-nlo').prop('checked', false);
    } else {
      $('#conf-int-nlo').prop('checked', true);
    }
    $('#conf-int-itt').val(p.getNIter());
    $('#conf-int-cpi').val(p.getNCalls());
    $('#conf-int-sqrts').val(p.getSqrts());
    $('.integrate-right').fadeIn('fast');
  });

  $(document).on('click', '.process-entry-sim', () => {
    simulate.activeProcessId = $(this).attr('process-id');
    const p = simulate.SimulateList[simulate.activeProcessId];
    // Fill simulate fields
    $('#conf-sim-sim').prop('checked', p.getStatus());
    $('#conf-sim-events').val(p.getEvents());
    // Fill histogram fields
    simulate.Simulate.fillHistogramFieldsHTML();
    // Process selected show right column
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

  // Tab preview clicked, generate script
  $('#tab_button_preview').click(() => {
    backend.rebuildPreviewTab();
  });

  //  Changing tab, rebuild process list
  $('#tab_button_integrate, #tab_button_simulate, #tab_button_scan').click(() => {
    process.displayProcessList();
  });

  // Tab Cuts clicked, generate particles list
  $('#tab_button_cuts').click(() => {
    cuts.cutsClosure.rebuildParticlesHTML();
  });

  // Tab Simulate clicked, generate particles popup list
  $('#tab_button_simulate').click(() => {
    simulate.Simulate.rebuildParticlesHTML();
  });

  // Clicking on the model
  $(document).on('click', '.model', () => {
    $('#conf-model').html($(this).text() + ' <span class="caret"></span>');
  });

  //  Remove Alias
  $(document).on('click', '.alias-remove', () => {
    const id = $(this).attr('alias-id');
    alias.ExternalSindarinList.splice(id, 1);
    alias.rebuildAliasList();
  });

  // Button: Save Sindarin
  $('.savesin').click(() => {
    const SindarinScript = backend.rebuildVariables();
    $.post('/savesin', {src: SindarinScript}, (data) => {
      backend.messageGUI(data, 'alert-success');
    });
  });

  // Button: Run Whizard
  $('.runwhiz').click(() => {
    // Run option: [--rebuild-events, --rebuild-grids, --rebuild]
    const option = (typeof $(this).attr('opt') !== 'undefined') ?
      $(this).attr('opt') : '';

    // Animation
    $('#pbar').show();
    $('#whizoutput').fadeOut('fast');
    $('.outputcontainer').fadeOut('fast');

    // Functionality
    $('.runwhiz, .runarrow').attr('disabled', 'disabled');
    const SindarinScript = backend.rebuildVariables();
    let whizRunning = true;
    generic.monitorLogChanges(whizRunning);

    $.post('/runwhiz', {src: SindarinScript, option: option}, (data) => {
      // Animation
      $('.outputcontainer').fadeIn('fast');
      $('#pbar').fadeOut('fast');

      // Functionality
      whizRunning = false;
      $('.runwhiz, .runarrow').removeAttr('disabled');
      // Display output whizard file
      $('#whizoutput').load('whizard.log').fadeIn('fast');
      // Display pdf (assuming there exists one for now)
      $('#out_hist').html(
          '<embed src="whizard_analysis.pdf" width="100%" height="700px">');

      // Whiz->GUI error parser
      // AM: check for other keywords, change method
      const CritKeyword = 'FATAL ERROR:';
      const str = $.ajax({url: 'whizard.log', async: false}).responseText;

      if (str.indexOf(CritKeyword) > -1) {
        const s = str.substring(str.lastIndexOf(CritKeyword),
            str.lastIndexOf('*')).replace(/\*/g, '');
        if (s) backend.messageGUI(s, 'alert-danger');
      }
    });
  });

  // Design stuff
  $('[rel=popover]').popover({
    html: true,
    content: () => $('#pop_models').html(),
  });

  $('[rel=popover_aliases]').popover({
    html: true,
    content: () => $('#pop_aliases').html(),
  });

  $('[rel=popover_process]').popover({
    html: true,
    content: () => $('#pop_process').html(),
  });

  // Popover for histogram
  $('[data-toggle=popover_simulation_hist]').popover({
    html: true,
    content: () => $('#pop_sim_subevent').html(),
  });
});
