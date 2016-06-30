const process = require('../lib/process.js');
const expect = require('chai').expect;
const chai = require('chai');
const chaiAsPromised = require('chai-as-promised');
chai.use(chaiAsPromised);

describe('SindarinProcess', () => {
  it('should be constructable', () => {
    const incoming = 'e1, E1';
    const outgoing = 'e2, E2';
    const sindarinProcess = new process.SindarinProcess(incoming, outgoing);
    expect(sindarinProcess.counter).to.equal(0);
    expect(sindarinProcess.incoming).to.equal(incoming);
    expect(sindarinProcess.outgoing).to.equal(outgoing);
    expect(sindarinProcess.isNlo()).to.equal(false);
  });
  it('should be able to write itself as sindarin', () => {
    const incoming = 'e1, E1';
    const outgoing = 'e2, E2';
    const sindarinProcess = new process.SindarinProcess(incoming, outgoing);
    sindarinProcess.counter = 2;
    const expectName = 'proc_2 = e1, E1 => e2, E2';
    expect(sindarinProcess.name()).to.equal(expectName);
    sindarinProcess.setSqrts('500 GeV');
    sindarinProcess.setNIter(5);
    sindarinProcess.setNCalls('100');
    expect(sindarinProcess.writeToSindarin()).to.equal('process ' + expectName +
        '\nsqrts = 500 GeV' +
        '\nintegrate(proc_2) {iterations=5:100:"gw"}\n');
  });
});
