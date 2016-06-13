const simulation = require('./tabs.simulate');
const generic = require('./generic');
const scan = require('./scan');

export const ProcessList = [];


function SimulationData() {
  this.nEvents = 0;
}


function IntegrationData() {
  this.nlo = false;
  // Default values, should be set in a file
  this.sqrts = 0;
  this.nCalls = 10000;
  this.nIter = 5;
}


function ScanData() {
  this.Sets = [];
  this.type = '';
  this.title = '';
  this.xlabel = '';
  this.ylabel = '';
  this.xmin = '';
  this.xmax = '';
}


function SindarinProcessIsNlo() {
  return this.integrationData.nlo;
}


function SindarinProcessSetNlo(value) {
  this.integrationData.nlo = value;
}


function SindarinProcessGetSqrts() {
  return this.integrationData.sqrts;
}


function SindarinProcessSetSqrts(sqrts) {
  this.integrationData.sqrts = sqrts;
}


function SindarinProcessGetNIter() {
  return this.integrationData.nIter;
}


function SindarinProcessSetNIter(nIt) {
  this.integrationData.nIter = nIt;
}


function SindarinProcessGetNCalls() {
  return this.integrationData.nCalls;
}


function SindarinProcessSetNCalls(nCalls) {
  this.integrationData.nCalls = nCalls;
}


function SindarinProcessSetNEvents(nEv) {
  this.simulationData.nEvents = nEv;
}


function SindarinProcessGetNEvents() {
  return this.simulationData.nEvents;
}


export function extAssignScans() {
  for (let i = 0; i < ProcessList.length; i++) {
    ProcessList[i].grabScanData(i);
  }
}


function SindarinProcessToString() {
  try {
    if (this.counter <= 0) {
      throw new Error('Invalid process counter');
    }
    let str = 'process proc_' + this.counter + ' = '
      + this.incoming + ' => ' + this.outgoing;
    if (this.isNlo()) {
      str += " {nlo_calculation = 'Full'}";
    }
    return str;
  } catch (err) {
    console.error(err);
  }
}


function ProcessDisplayName() {
  return 'proc_' + this.counter + ' = ' + this.incoming + ' => ' + this.outgoing;
}


function grabScanData(processID) {
  this.ScanData.Sets = scan.ScansList[processID].ScansContainer;
  this.ScanData.type = scan.ScansList[processID].type;
  this.ScanData.title = scan.ScansList[processID].title;
  this.ScanData.xlabel = scan.ScansList[processID].xlabel;
  this.ScanData.ylabel = scan.ScansList[processID].ylabel;
  this.ScanData.xmin = scan.ScansList[processID].xmin;
  this.ScanData.xmax = scan.ScansList[processID].xmax;
}


function SindarinProcess(incoming, outgoing) {
  this.counter = 0;
  this.incoming = incoming;
  this.outgoing = outgoing;
  this.integrationData = new IntegrationData();
  this.simulationData = new SimulationData();
  this.ScanData = new ScanData();
  this.isNlo = SindarinProcessIsNlo;
  this.setNlo = SindarinProcessSetNlo;
  this.setSqrts = SindarinProcessSetSqrts;
  this.getSqrts = SindarinProcessGetSqrts;
  this.getNIter = SindarinProcessGetNIter;
  this.setNIter = SindarinProcessSetNIter;
  this.getNCalls = SindarinProcessGetNCalls;
  this.setNCalls = SindarinProcessSetNCalls;
  this.getNEvents = SindarinProcessGetNEvents;
  this.setNEvents = SindarinProcessSetNEvents;
  this.toString = SindarinProcessToString;
  this.name = ProcessDisplayName;
  this.grabScanData = grabScanData;
}


export function SindarinWriteProcesses() {
  for (let i = 0; i < this.nElements; i++) {
    const p = this.list[i];
    if (p instanceof SindarinProcess) {
      this.src += this.list[i].toString() + '\n';
      // If scans defined overwrite
      if (p.ScanData.Sets.length > 0) {
        this.src += '#Plot data' + '\n';
        this.src += '$x_label = "' + p.ScanData.xlabel + '"' + '\n';
        this.src += '$y_label = "' + p.ScanData.ylabel + '"' + '\n';
        this.src += '$title = "' + p.ScanData.title + '"' + '\n';
        this.src += 'plot lineshape_' + i + ' { x_min = ' +
          p.ScanData.xmin + ' x_max = ' + p.ScanData.xmax + ' }' + '\n';
        this.src += 'scan sqrts = (';
        for (let j = 0; j < p.ScanData.Sets.length; j++) {
          const e = p.ScanData.Sets[j];
          this.src += '(' + e.min + ' => ' + e.max + ' /+ ' + e.inc + '),';
        }
        this.src = this.src.substring(0, this.src.length - 1);
        this.src += ') {' + '\n';
        this.src += '\tintegrate (proc_' + p.counter +
            ') { iterations = 2:1000:"gw", 1:2000 }' + '\n';
        this.src += 'record lineshape_' + i +
          '(sqrts, integral (proc_' + i + ') / 1000)' + '\n';
        this.src += '}\ncompile_analysis ';
      } else {
        this.src += 'sqrts = ' + this.list[i].getSqrts() + '\n';
        if (p.getNCalls() > 0 && p.getNIter() > 0) {
          this.src += 'integrate(proc_' + p.counter + ') {iterations=' +
            p.getNIter() + ':' + p.getNCalls() + ':"gw"}\n';
        }
      }
      if (p.getNEvents() > 0) {
        this.src += 'simulate(proc_' + p.counter +
            ') {n_events=' + p.getNEvents() + '}\n';
      }
      this.elementsUsed.push(i);
    }
  }
}

export function rebuildProcessList() {
  $('#pop_process').empty();
  $('#pop_process').append('<div class="row">');
  let procIndex = 1;
  for (let i = 0; i < ProcessList.length; i++) {
    if (ProcessList[i] instanceof SindarinProcess) {
      if (ProcessList[i] === null) continue; // !!! // ???
      ProcessList[i].counter = procIndex;
      procIndex++;
      $('#pop_process').append(
          '<div class="col-md-10"><a href="javascript:;" class="process">' +
          ProcessList[i].name() + '</a></div><div class="col-md-2">' +
          '<a href="javascript:;" class="process-remove" process-id=' + i +
          '><span class="glyphicon glyphicon-remove-sign" aria-hidden="true">' +
          '</span></a></div>');
    }
    $('#pop_process').append('</div>');
  }
}

// Add a new process
export function addProcess(incoming, outgoing) {
  ProcessList.push(new SindarinProcess(incoming, outgoing));
  simulation.addSimulation();
  scan.Scan.newProcess();
  rebuildProcessList();
}

// Generate process list to choose setups from
export function displayProcessList() {
  /*
   * Constructing integration list
   */
  $('#integrate-process-list').empty();
  for (let i = 0; i < ProcessList.length; i++) {
    if (ProcessList[i] === null) continue;
    const ip1 = i + 1;
    const name = generic.texImageOrPlain(ProcessList[i].name());
    $('#integrate-process-list').append(
        '<a href="#" class="list-group-item process-entry" process-id="' + ip1 + '">' +
        name + '</a>');
  }

  /*
   *  Constructing simulation list
   */
  $('#simulate-process-list').empty();
  for (let i = 0; i < ProcessList.length; i++) {
    if (ProcessList[i] === null) continue;
    const CSSClass = simulation.SimulateList[i].status ? 'label-success' : 'label-default';
    const Text = simulation.SimulateList[i].status ? 'On' : 'Off';
    const name = generic.texImageOrPlain(ProcessList[i].name());
    $('#simulate-process-list').append(
        '<a href="#" class="list-group-item process-entry-sim" process-id="' +
        i + '">' + name + '<br><span id="proc_indicator_' + i +
        '" class="label ' + CSSClass + '">' + Text + '</span></a>');
  }

  // Constructing process list for scan
  $('#scan-process-list').empty();
  for (let i = 0; i < ProcessList.length; i++) {
    if (ProcessList[i] === null) continue;
    const CSSClass = scan.ScansList[i].status ? 'label-success' : 'label-default';
    const Text = scan.ScansList[i].status ? 'On' : 'Off';
    const name = generic.texImageOrPlain(ProcessList[i].name());
    $('#scan-process-list').append(
        '<a href="#" class="list-group-item process-entry-scan" process-id="' +
        i + '">' + name + '<br><span id="proc_indicator_scan_' + i +
        '" class="label ' + CSSClass + '">' + Text + '</span></a>');
  }

  // If no process added, suggest adding one
  if (ProcessList.filter((value) => value !== null).length === 0) {
    $('#simulate-process-list').html('Please add a process.');
    $('#integrate-process-list').html('Please add a process.');
    $('#scan-process-list').html('Please add a process.');
    $('.simulate-right, .integrate-right, .scan-right').hide();
  }
}
