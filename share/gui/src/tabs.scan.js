const simulate = require('./tabs.simulate');

// Hiding optional fields
$('#struct-scan').hide();

/*
 * Object structure:
 * ScansList[i] : contain all Scan information for a i'th process
 * ScansList[i].ScansContainer : contains union of subintervals for a i'th process
 * ScansList[i].ScansContainer[j] : contains j'th individual subinterval for
 *                          i'th process ex: (88.0 GeV => 90.0 GeV /+ 0.5 GeV)
 */

export const ScansList = [];

function ScanElement(min, max, inc) {
  this.min = min;
  this.max = max;
  this.inc = inc;
}

function addScanElement(min, max, inc) {
  this.ScansContainer.push(new ScanElement(min, max, inc));
}

function ScanProcess() {
  this.ScansContainer = [];
  this.status = false;
  this.type;
  this.title;
  this.xlabel;
  this.ylabel;
  this.xmin;
  this.xmax;
  this.addScanElement = addScanElement;
}

// function addScanProcess() {
//   Scans.addScanProcess();
// }

export const Scan = {
  newProcess: () => {
    ScansList.push(new ScanProcess());
  },

  // Rebuilding Scan configuration for a selected process
  FillHTMLFields: () => {
    Scan.Clean();
    for (let i = 0; i <
        ScansList[simulate.activeProcessId].ScansContainer.length; i++) {
      const elem = ScansList[simulate.activeProcessId].ScansContainer[i];
      Scan.AddNew(elem.min, elem.max, elem.inc, i);
    }
    $('#conf-scan-check').prop('checked', ScansList[simulate.activeProcessId].status);
    $('#scan-plot-title').val(ScansList[simulate.activeProcessId].title);
    $('#scan-plot-xlabel').val(ScansList[simulate.activeProcessId].xlabel);
    $('#scan-plot-ylabel').val(ScansList[simulate.activeProcessId].ylabel);
    $('#scan-plot-xmin').val(ScansList[simulate.activeProcessId].xmin);
    $('#scan-plot-xmax').val(ScansList[simulate.activeProcessId].xmax);
  },

  getScanCodeHTML: (min, max, inc, sid) =>
    '<div class="row" rel="scan-elem" scanid="' + sid + '"> \
        <div class="col-sm-4"> \
          <div class="form-group"> \
            <input type="text" class="form-control conf-scan-min" \
            id="" placeholder="Min value" value="'+min+'"> \
          </div> \
        </div>  \
        <div class="col-sm-4"> \
          <div class="form-group"> \
            <input type="text" class="form-control conf-scan-max" \
            id="" placeholder="Max value" value="'+max+'"> \
          </div> \
        </div>         \
        <div class="col-sm-4"> \
          <div class="form-group"> \
            <input type="text" class="form-control conf-scan-inc" \
            id="" placeholder="Increment" value="'+inc+'"> \
          </div> \
        </div>  \
      </div>',

  AddNew: (min, max, inc, sid) => {
    $('#scansContainer').append(Scan.getScanCodeHTML(min, max, inc, sid));
  },

  Clean: () => {
    $('#scansContainer').html('');
  },
};

// Selecting: Scan > Process
$(document).on('click', '.process-entry-scan', () => {
  $('.scan-right').fadeIn('fast');
  $('.process-entry-scan').removeClass('active');
  $(this).addClass('active');
  simulate.activeProcessId = $(this).attr('process-id');
  Scan.FillHTMLFields();
  if (ScansList[simulate.activeProcessId].status) {
    $('#struct-scan').fadeIn('fast');
  } else {
    $('#struct-scan').fadeOut('fast');
  }
});

// Button: New Scan subinterval
$('.scan-newscan').click(() => {
  Scan.AddNew('', '', '',
      ScansList[simulate.activeProcessId].ScansContainer.length);
  ScansList[simulate.activeProcessId].addScanElement(0, 0, 0);
});

// Button: Clean Scans
$('.scan-clean').click(() => {
  ScansList[simulate.activeProcessId].ScansContainer = [];
  Scan.Clean();
});

// Checkbox button: Enable scan for this process
$('#conf-scan-check').click(() => {
  ScansList[simulate.activeProcessId].status = $(this).prop('checked');
  if ($(this).prop('checked')) {
    $('#struct-scan').fadeIn('fast');
  } else {
    $('#struct-scan').fadeOut('fast');
  }
  // Changing On/Off indicator
  if (ScansList[simulate.activeProcessId].status) {
    $('#proc_indicator_scan_' + simulate.activeProcessId).removeClass('label-default label-success').addClass('label-success').text('On');
  } else{
    $('#proc_indicator_scan_' + simulate.activeProcessId).removeClass('label-default label-success').addClass('label-default').text('Off');
  }
});

// Modify field: Scan-minimum value
$(document).on('change', '.conf-scan-min', () => {
  const min = $(this).val();
  const intervalID = $(this).parent().parent().parent().attr('scanid');
  ScansList[simulate.activeProcessId].ScansContainer[intervalID].min = min;
});

// Modify field: Scan-maximum value
$(document).on('change', '.conf-scan-max', () => {
  const max = $(this).val();
  const intervalID = $(this).parent().parent().parent().attr('scanid');
  ScansList[simulate.activeProcessId].ScansContainer[intervalID].max = max;
});

// Modify field: Scan-inc value
$(document).on('change', '.conf-scan-inc', () => {
  const inc = $(this).val();
  const intervalID = $(this).parent().parent().parent().attr('scanid');
  ScansList[simulate.activeProcessId].ScansContainer[intervalID].inc = inc;
});

// Plot fields modification
$('#scan-plot-title').change(() => {
  ScansList[simulate.activeProcessId].title = $('#scan-plot-title').val();
});

$('#scan-plot-xlabel').change(() => {
  ScansList[simulate.activeProcessId].xlabel = $('#scan-plot-xlabel').val();
});

$('#scan-plot-ylabel').change(() => {
  ScansList[simulate.activeProcessId].ylabel = $('#scan-plot-ylabel').val();
});

$('#scan-plot-xmin').change(() => {
  ScansList[simulate.activeProcessId].xmin = $('#scan-plot-xmin').val();
});

$('#scan-plot-xmax').change(() => {
  ScansList[simulate.activeProcessId].xmax = $('#scan-plot-xmax').val();
});
