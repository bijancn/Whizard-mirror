const models = require('./models');
const alias = require('./alias');
const simulation = require('./simulation');
const cuts = require('./cuts');


function SindarinAssignmentToString() {
  return this.identifier + ' = ' + this.value;
}


export function SindarinAssignment(str, val) {
  this.identifier = str;
  this.value = val;
  this.toString = SindarinAssignmentToString;
}


function SindarinCommandToString() {
  return this.command;
}


export function SindarinCommand(str) {
  this.command = str;
  this.toString = SindarinCommandToString;
}


export function SindarinWriteHeader() {
  this.src += '# This Sindarin script was automatically generated by the Whizard-GUI\n';
  const date = new Date();
  this.src += '# Date: ' + (date.getMonth() + 1) + '-' +
    date.getDate() + '-' + date.getFullYear();
  this.src += ', ' + date.getHours() + ':' + date.getMinutes() + 'h\n';
  this.src += '\n';
}


// TODO: (bcn 2016-04-17) use reduce for this
function SindarinWriteAdditionalCode() {
  for (let i = 0; i < this.list.length; i++) {
    const elem = this.list[i];
    // TODO: (bcn 2016-06-29) BIG TODO CONTINUE HERE
    // all the elements of this.list which comes from SindarinList in backend.js
    // should have a writeToSindarin or toString method and then one can just
    // iterate over all of them with one loop without ugly instanceof
    // eslint-disable-next-line no-use-before-define
    if (elem instanceof SindarinAdditionalCode) {
      this.src += elem.as + '\n';
    }
  }
}


export function SindarinAdditionalCode(s) {
  this.as = s;
  // TODO: (bcn 2016-06-29) this looks useless and in the wrong place
  // this.writeModelData = SindarinWriteAdditionalCode;
}


function SindarinWriteBody() {
  for (let i = 0; i < this.nElements; i++) {
    if (this.elementsUsed.indexOf(i) < 0) {
      this.src += this.list[i].toString() + '\n';
      this.elementsUsed.push(i);
    }
  }
}


function constructSindarinFromList() {
  this.writeHeader();
  this.writeAdditionalData();
  this.writeModelData();
  this.writeAliases();
  this.writeCuts();
  this.writeProcesses();
  this.writeSimulate();
  return this.src;
}


export function SindarinWriteProcesses() {
  for (let i = 0; i < this.nElements; i++) {
    const p = this.list[i];
    this.src += p.writeProcess(i);
    this.elementsUsed.push(i);
  }
}


// TODO: (bcn 2016-04-17) this cant be unit tested for now as it depends on too much
export function SindarinGenerator(SindarinList) {
  this.list = SindarinList;
  this.src = '';
  this.nElements = SindarinList.length;
  // We will use this array to save the indices of the elements which have already been processed.
  this.elementsUsed = [];
  this.construct = constructSindarinFromList;
  this.writeHeader = SindarinWriteHeader;
  this.writeModelData = models.SindarinWriteModelData;
  this.writeAdditionalData = SindarinWriteAdditionalCode;
  this.writeAliases = alias.SindarinWriteAliases;
  this.writeProcesses = SindarinWriteProcesses;
  this.writeBody = SindarinWriteBody;
  this.writeSimulate = simulation.SindarinWriteSimulate;
  this.writeCuts = cuts.SindarinWriteCuts;
}
